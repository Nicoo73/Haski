-- src/Enemy.hs

module Enemy
  ( Enemy(..)
  , EnemyType(..)
  , Direction(..)
  , updateEnemy
  , enemySpeed
  , enemyRadius
  , createCaza
  , shouldEnemyShoot
  , vectorToDirection
  ) where

import System.Random (randomRIO)

-------------------------------------------------------------
-- DIRECCIONES (Definido aquí para evitar dependencia circular)
-------------------------------------------------------------

data Direction = DUp | DDown | DLeft | DRight 
               | DUpLeft | DUpRight | DDownLeft | DDownRight
  deriving (Eq, Show)

-------------------------------------------------------------
-- TIPOS DE ENEMIGOS
-------------------------------------------------------------

data EnemyType = Caza
  deriving (Eq, Show)

-------------------------------------------------------------
-- DATOS DE UN ENEMIGO
-------------------------------------------------------------

data Enemy = Enemy
  { enemyPos         :: (Float, Float)
  , enemySpeed       :: Float
  , enemyType        :: EnemyType
  , enemyHealth      :: Int
  , enemyDamage      :: Int
  , enemyBulletSpd   :: Float
  , enemyShootTimer  :: Float  -- Time until next shot
  , enemyShootRange  :: Float  -- Maximum shooting range (110% of player range)
  , enemyMinRange    :: Float  -- Minimum desired range (90% of player range)
  } deriving (Show)

enemyRadius :: Float
enemyRadius = 12.0 -- Definir el radio para las colisiones (círculo sólido en Render.hs)

-------------------------------------------------------------
-- CONSTRUCTORES DE ENEMIGOS
-------------------------------------------------------------

-- Caza: 5 HP, 2 daño, 90% velocidad movimiento, 10% velocidad proyectil
-- Rango de disparo: 50%-110% del rango del jugador (más flexible)
createCaza :: (Float, Float) -> Float -> Float -> Float -> Enemy
createCaza pos baseMoveSpeed baseBulletSpeed playerRange = Enemy
  { enemyPos        = pos
  , enemySpeed      = baseMoveSpeed * 0.9  -- 90% velocidad del jugador
  , enemyType       = Caza
  , enemyHealth     = 5
  , enemyDamage     = 2
  , enemyBulletSpd  = baseBulletSpeed * 0.1  -- 10% velocidad de bala del jugador
  , enemyShootTimer = 0.0
  , enemyShootRange = playerRange * 1.1  -- 110% del rango del jugador
  , enemyMinRange   = playerRange * 0.5  -- 50% del rango del jugador (más cercano)
  }

-------------------------------------------------------------
-- UTILIDADES
-------------------------------------------------------------

-- Convert movement vector to Direction (8 directions)
vectorToDirection :: (Float, Float) -> Direction
vectorToDirection (dx, dy)
  | dx > 0.3 && dy > 0.3   = DUpRight
  | dx < -0.3 && dy > 0.3  = DUpLeft
  | dx > 0.3 && dy < -0.3  = DDownRight
  | dx < -0.3 && dy < -0.3 = DDownLeft
  | dy > 0.3               = DUp
  | dy < -0.3              = DDown
  | dx < -0.3              = DLeft
  | dx > 0.3               = DRight
  | otherwise              = DUp  -- Default

-------------------------------------------------------------
-- SISTEMA DE DISPARO
-------------------------------------------------------------

-- Determina si el enemigo debe disparar (basado en cooldown)
shouldEnemyShoot :: Float -> Enemy -> IO (Bool, Enemy)
shouldEnemyShoot dt enemy = do
  let timer = enemyShootTimer enemy
  if timer <= 0
    then do
      -- Cooldown fijo de 1 segundo entre disparos
      let newCooldown = 1.0
      return (True, enemy { enemyShootTimer = newCooldown })
    else
      return (False, enemy { enemyShootTimer = timer - dt })

-- Función que calcula la fuerza de separación (NEW)
-- Genera un vector que empuja al enemigo 'e' lejos de sus vecinos 'neighbors'
calculateSeparationForce :: [Enemy] -> Enemy -> (Float, Float)
calculateSeparationForce neighbors enemy = foldr addVector (0, 0) separationVectors
  where
    (ex, ey) = enemyPos enemy
    -- Distancia segura antes de que los enemigos se empujen (suma de radios)
    separationDistance = 2 * enemyRadius 
    
    -- Calcula el vector de fuerza de separación para cada vecino
    separationVectors = [ vectorToNeighbor neighbor | neighbor <- neighbors, isTooClose neighbor ]

    isTooClose neighbor = 
        let (nx, ny) = enemyPos neighbor
            distSq = (nx - ex)^2 + (ny - ey)^2
        in distSq < separationDistance * separationDistance && distSq > 0.1 -- Evita dividr por cero
        
    vectorToNeighbor neighbor =
        let (nx, ny) = enemyPos neighbor
            dx = ex - nx  -- Vector que apunta desde el vecino hacia este enemigo
            dy = ey - ny
            dist = sqrt (dx*dx + dy*dy)
            -- Fuerza de empuje inversamente proporcional a la distancia (más cerca, más fuerte)
            forceFactor = 1.0 / dist 
        in (dx * forceFactor, dy * forceFactor) 

    addVector (a,b) (c,d) = (a+c, b+d)

-- Actualiza la posición del enemigo con lógica de mantener distancia de disparo
updateEnemy :: Float -> (Float, Float) -> [Enemy] -> Enemy -> Enemy
updateEnemy dt (px, py) allEnemies enemy =
  let (ex, ey) = enemyPos enemy
      
      -- Calcular distancia al jugador
      dx_player = px - ex
      dy_player = py - ey
      dist_player = sqrt (dx_player*dx_player + dy_player*dy_player)
      
      -- Rangos del enemigo
      maxRange = enemyShootRange enemy  -- 110% rango del jugador
      minRange = enemyMinRange enemy    -- 90% rango del jugador
      optimalRange = (maxRange + minRange) / 2  -- 100% rango del jugador
      
      -- 1. COMPORTAMIENTO BASADO EN DISTANCIA
      (vx_behavior, vy_behavior) = 
        if dist_player > maxRange then
          -- Demasiado lejos: acercarse al jugador
          if dist_player > 0
            then (dx_player / dist_player, dy_player / dist_player)
            else (0, 0)
        else if dist_player < minRange then
          -- Demasiado cerca: alejarse del jugador
          if dist_player > 0
            then (-dx_player / dist_player, -dy_player / dist_player)
            else (0, 0)
        else
          -- En rango óptimo: mantener posición (movimiento lateral)
          -- Orbitar alrededor del jugador para mantener distancia
          if dist_player > 0
            then 
              let perpX = -dy_player / dist_player  -- Vector perpendicular
                  perpY = dx_player / dist_player
                  -- Pequeño ajuste hacia el rango óptimo
                  towardsOptimal = (optimalRange - dist_player) / optimalRange
                  adjustX = (dx_player / dist_player) * towardsOptimal * 0.3
                  adjustY = (dy_player / dist_player) * towardsOptimal * 0.3
              in (perpX * 0.7 + adjustX, perpY * 0.7 + adjustY)
            else (0, 0)
      
      -- 2. FUERZA DE SEPARACIÓN (SEPARATION - de otros enemigos)
      otherEnemies = filter (\e -> enemyPos e /= enemyPos enemy) allEnemies
      (vx_sep, vy_sep) = calculateSeparationForce otherEnemies enemy
      
      -- 3. COMBINAR FUERZAS
      vx_final = (vx_behavior * 1.5) + (vx_sep * 1.0)
      vy_final = (vy_behavior * 1.5) + (vy_sep * 1.0)
      
      -- Normalizar el vector de movimiento final
      magnitude = sqrt (vx_final*vx_final + vy_final*vy_final)
      (vx_norm, vy_norm) = if magnitude > 0
                           then (vx_final / magnitude, vy_final / magnitude)
                           else (0, 0)
                           
      speed = enemySpeed enemy
      rawNewX = ex + vx_norm * speed * dt
      rawNewY = ey + vy_norm * speed * dt
      
      -- Aplicar límites del terreno (640x360) con margen de 16px
      terrainW = 640.0  -- Actualizado al nuevo ancho
      terrainH = 360.0
      margin = 16.0  -- Margen para evitar pegarse al borde
      clampedX = max (-terrainW / 2 + margin) (min (terrainW / 2 - margin) rawNewX)
      clampedY = max (-terrainH / 2 + margin) (min (terrainH / 2 - margin) rawNewY)
      newPos = (clampedX, clampedY)
      
      -- Actualizar temporizador de disparo
      newTimer = max 0 (enemyShootTimer enemy - dt)
      
  in enemy { enemyPos = newPos, enemyShootTimer = newTimer }