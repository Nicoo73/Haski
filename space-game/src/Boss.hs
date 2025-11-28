-- src/Boss.hs
-- Final Boss system with attack patterns

module Boss
  ( Boss(..)
  , BossAttack(..)
  , AttackType(..)
  , createBoss
  , updateBoss
  , bossRadius
  ) where

import Enemy (Direction(..), vectorToDirection)

-------------------------------------------------------------
-- TIPOS DE ATAQUE
-------------------------------------------------------------

data AttackType = AT1Arrows | AT2Ball
  deriving (Eq, Show)

-- Un ataque activo en el mundo
data BossAttack = BossAttack
  { attackType :: AttackType
  , attackPos  :: (Float, Float)
  , attackDir  :: Direction  -- Dirección del movimiento
  , attackSpeed :: Float
  , attackDamage :: Int
  , attackPhase :: Int  -- Para AT1: 0=mitad superior, 1=mitad inferior
  } deriving (Show)

-------------------------------------------------------------
-- DATOS DEL BOSS
-------------------------------------------------------------

data Boss = Boss
  { bossPos :: (Float, Float)
  , bossHealth :: Int
  , bossMaxHealth :: Int
  , bossSpeed :: Float
  , bossShootRange :: Float
  , bossMinRange :: Float
  , attackPattern :: [AttackType]  -- Patrón de ataques pendientes
  , currentAttackType :: Maybe AttackType  -- Ataque actual en ejecución
  , attackRepeatCount :: Int  -- Repeticiones del ataque actual
  , attackCooldown :: Float  -- Tiempo hasta próximo ataque
  , at1Phase :: Int  -- Fase del AT1 (0=superior, 1=inferior)
  , at1Direction :: Direction  -- Dirección de AT1 (DRight o DLeft)
  } deriving (Show)

bossRadius :: Float
bossRadius = 32.0

-------------------------------------------------------------
-- CONSTRUCTOR DEL BOSS
-------------------------------------------------------------

createBoss :: Boss
createBoss = Boss
  { bossPos = (0, 150)  -- Arriba en el centro
  , bossHealth = 1000
  , bossMaxHealth = 1000
  , bossSpeed = 30.0  -- 30 px/s
  , bossShootRange = 252.0  -- 100% del rango del jugador (252px)
  , bossMinRange = 126.0  -- 50% del rango del jugador
  , attackPattern = initialPattern
  , currentAttackType = Nothing
  , attackRepeatCount = 0
  , attackCooldown = 1.0  -- Espera 1 segundo antes del primer ataque
  , at1Phase = 0
  , at1Direction = DRight
  }
  where
    -- Patrón: AT2x10, AT1x1, AT2x5, AT2x10, AT1x1 (invertido), AT2x5
    initialPattern = 
      replicate 10 AT2Ball ++
      [AT1Arrows] ++
      replicate 5 AT2Ball ++
      replicate 10 AT2Ball ++
      [AT1Arrows] ++
      replicate 5 AT2Ball

-------------------------------------------------------------
-- ACTUALIZACIÓN DEL BOSS
-------------------------------------------------------------

updateBoss :: Float -> (Float, Float) -> Boss -> (Boss, [BossAttack])
updateBoss dt playerPos boss =
  -- 1. Mover el boss (comportamiento táctico similar a enemigos)
  let movedBoss = moveBoss dt playerPos boss
      
      -- 2. Procesar ataques
      (finalBoss, attacks) = 
        if attackCooldown movedBoss > 0
        then (movedBoss { attackCooldown = attackCooldown movedBoss - dt }, [])
        else case currentAttackType movedBoss of
          Nothing -> selectNextAttack movedBoss
          Just AT2Ball -> 
            -- Solo disparar AT2 si está en rango
            let (bx, by) = bossPos movedBoss
                (px, py) = playerPos
                dx = px - bx
                dy = py - by
                dist = sqrt (dx*dx + dy*dy)
            in if dist <= bossShootRange movedBoss
               then executeAT2 dt playerPos movedBoss
               else (movedBoss, [])  -- Esperar hasta estar en rango
          Just AT1Arrows -> executeAT1 dt movedBoss
  
  in (finalBoss, attacks)

-- Movimiento del boss (comportamiento táctico)
moveBoss :: Float -> (Float, Float) -> Boss -> Boss
moveBoss dt (px, py) boss =
  let (bx, by) = bossPos boss
      
      -- Calcular distancia al jugador
      dx = px - bx
      dy = py - by
      dist = sqrt (dx*dx + dy*dy)
      
      maxRange = bossShootRange boss
      minRange = bossMinRange boss
      optimalRange = (maxRange + minRange) / 2
      
      -- Comportamiento táctico
      (vx, vy) = if dist > maxRange then
        -- Demasiado lejos: acercarse
        if dist > 0 then (dx / dist, dy / dist) else (0, 0)
      else if dist < minRange then
        -- Demasiado cerca: alejarse
        if dist > 0 then (-dx / dist, -dy / dist) else (0, 0)
      else
        -- En rango óptimo: orbitar
        if dist > 0
        then 
          let perpX = -dy / dist
              perpY = dx / dist
              towardsOptimal = (optimalRange - dist) / optimalRange
              adjustX = (dx / dist) * towardsOptimal * 0.3
              adjustY = (dy / dist) * towardsOptimal * 0.3
          in (perpX * 0.7 + adjustX, perpY * 0.7 + adjustY)
        else (0, 0)
      
      -- Aplicar movimiento
      speed = bossSpeed boss
      rawNewX = bx + vx * speed * dt
      rawNewY = by + vy * speed * dt
      
      -- Límites del terreno
      terrainW = 640.0
      terrainH = 360.0
      margin = 32.0  -- Margen considerando el tamaño del boss
      clampedX = max (-terrainW / 2 + margin) (min (terrainW / 2 - margin) rawNewX)
      clampedY = max (-terrainH / 2 + margin) (min (terrainH / 2 - margin) rawNewY)
      
  in boss { bossPos = (clampedX, clampedY) }

-------------------------------------------------------------
-- SELECCIÓN DEL SIGUIENTE ATAQUE
-------------------------------------------------------------

selectNextAttack :: Boss -> (Boss, [BossAttack])
selectNextAttack boss =
  case attackPattern boss of
    [] -> 
      -- Patrón completo, reiniciar
      let newBoss = boss { attackPattern = initialPattern boss }
      in selectNextAttack newBoss
    
    (nextAttack : rest) ->
      -- Contar cuántos ataques consecutivos del mismo tipo hay
      let sameTypeCount = 1 + length (takeWhile (== nextAttack) rest)
          newPattern = drop (sameTypeCount - 1) rest
          
          newBoss = boss
            { currentAttackType = Just nextAttack
            , attackRepeatCount = sameTypeCount
            , attackPattern = newPattern
            , attackCooldown = 0.5  -- Cooldown antes de lanzar
            }
      in (newBoss, [])

initialPattern :: Boss -> [AttackType]
initialPattern boss = 
  replicate 10 AT2Ball ++
  [AT1Arrows] ++
  replicate 5 AT2Ball ++
  replicate 10 AT2Ball ++
  [AT1Arrows] ++
  replicate 5 AT2Ball

-------------------------------------------------------------
-- EJECUCIÓN DE ATAQUES
-------------------------------------------------------------

-- AT2: Lanza una bola hacia el jugador
executeAT2 :: Float -> (Float, Float) -> Boss -> (Boss, [BossAttack])
executeAT2 dt (px, py) boss =
  let (bx, by) = bossPos boss
      dx = px - bx
      dy = py - by
      dist = sqrt (dx*dx + dy*dy)
      
      -- Dirección hacia el jugador
      dir = if dist > 0
            then vectorToDirection (dx/dist, dy/dist)
            else DDown
      
      -- Crear el ataque
      attack = BossAttack
        { attackType = AT2Ball
        , attackPos = bossPos boss
        , attackDir = dir
        , attackSpeed = 60.0  -- 60 px/s
        , attackDamage = 9
        , attackPhase = 0
        }
      
      -- Actualizar boss
      newRepeatCount = attackRepeatCount boss - 1
      newBoss = if newRepeatCount <= 0
                then boss { currentAttackType = Nothing, attackCooldown = 1.0 }
                else boss { attackRepeatCount = newRepeatCount, attackCooldown = 0.8 }
      
  in (newBoss, [attack])

-- AT1: Lanza flechas de un lado a otro
executeAT1 :: Float -> Boss -> (Boss, [BossAttack])
executeAT1 dt boss =
  let terrainW = 640.0
      terrainH = 360.0
      phase = at1Phase boss
      direction = at1Direction boss
      
      -- Determinar posición Y según la fase
      yPos = if phase == 0
             then terrainH / 4   -- Mitad superior (1/4 desde arriba)
             else -terrainH / 4  -- Mitad inferior (1/4 desde abajo)
      
      -- Determinar posición X inicial y dirección
      (xPos, dir) = case direction of
        DRight -> (-terrainW / 2 + 16, DRight)  -- Izquierda a derecha
        DLeft  -> (terrainW / 2 - 16, DLeft)    -- Derecha a izquierda
        _ -> (-terrainW / 2 + 16, DRight)
      
      -- Crear múltiples flechas en línea horizontal
      numArrows = 5  -- 5 flechas simultáneas
      arrowSpacing = 60.0  -- Espaciado entre flechas
      
      attacks = [ BossAttack
                    { attackType = AT1Arrows
                    , attackPos = (xPos, yPos + (fromIntegral i - 2) * arrowSpacing)
                    , attackDir = dir
                    , attackSpeed = 50.0  -- 50 px/s
                    , attackDamage = 20
                    , attackPhase = phase
                    }
                | i <- [0..numArrows-1] ]
      
      -- Actualizar fase y estado del boss
      newPhase = if phase == 0 then 1 else 0
      newRepeatCount = if newPhase == 0 then attackRepeatCount boss - 1 else attackRepeatCount boss
      
      -- Alternar dirección después de completar ambas fases
      newDirection = if newPhase == 0
                     then if direction == DRight then DLeft else DRight
                     else direction
      
      newBoss = if newRepeatCount <= 0 && newPhase == 0
                then boss 
                  { currentAttackType = Nothing
                  , attackCooldown = 2.0  -- Cooldown más largo después de AT1
                  , at1Phase = 0
                  , at1Direction = newDirection
                  }
                else boss
                  { attackRepeatCount = newRepeatCount
                  , attackCooldown = 3.0  -- Esperar 3 segundos entre fases
                  , at1Phase = newPhase
                  , at1Direction = newDirection
                  }
  
  in (newBoss, attacks)
