-- src/Enemy.hs

module Enemy
  ( Enemy(..)
  , EnemyType(..)
  , Direction(..)
  , updateEnemy
  , enemySpeed
  , enemyRadius
  , createAlien1
  , createAlien2
  , createAlien3
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

data EnemyType = Alien1 | Alien2 | Alien3
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

-- Alien1: 100 HP, 6 daño, 75 px/s movimiento, 66 px/s proyectil
-- Rango de disparo: 50%-100% del rango del jugador
createAlien1 :: (Float, Float) -> Float -> Float -> Float -> Enemy
createAlien1 pos _baseMoveSpeed _baseBulletSpeed playerRange = Enemy
  { enemyPos        = pos
  , enemySpeed      = 75.0  -- 75 px/s (velocidad absoluta)
  , enemyType       = Alien1
  , enemyHealth     = 100
  , enemyDamage     = 6
  , enemyBulletSpd  = 66.0  -- 66 px/s (velocidad absoluta)
  , enemyShootTimer = 0.0
  , enemyShootRange = playerRange * 1.0  -- 100% del rango del jugador (252 px)
  , enemyMinRange   = playerRange * 0.5  -- 50% del rango del jugador
  }

-- Alien2: 140 HP, 3 daño, 45 px/s movimiento, 45 px/s proyectil
-- Rango de disparo: 125% del rango del jugador (315 px)
createAlien2 :: (Float, Float) -> Float -> Float -> Float -> Enemy
createAlien2 pos _baseMoveSpeed _baseBulletSpeed playerRange = Enemy
  { enemyPos        = pos
  , enemySpeed      = 45.0  -- 45 px/s (velocidad absoluta)
  , enemyType       = Alien2
  , enemyHealth     = 140
  , enemyDamage     = 3
  , enemyBulletSpd  = 45.0  -- 45 px/s (velocidad absoluta)
  , enemyShootTimer = 0.0
  , enemyShootRange = playerRange * 1.25  -- 125% del rango del jugador (315 px)
  , enemyMinRange   = playerRange * 0.5  -- 50% del rango del jugador
  }

-- Alien3: 50 HP, 15 daño, 115 px/s movimiento (kamikaze)
-- Se estrella contra el jugador cuando está a menos de 16px
createAlien3 :: (Float, Float) -> Float -> Float -> Float -> Enemy
createAlien3 pos _baseMoveSpeed _baseBulletSpeed _playerRange = Enemy
  { enemyPos        = pos
  , enemySpeed      = 115.0  -- 115 px/s (velocidad absoluta)
  , enemyType       = Alien3
  , enemyHealth     = 50
  , enemyDamage     = 15
  , enemyBulletSpd  = 0  -- No dispara
  , enemyShootTimer = 999  -- Nunca dispara
  , enemyShootRange = 0  -- No tiene rango de disparo
  , enemyMinRange   = 0  -- Se acerca sin límite
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
      -- Cooldown fijo de 2 segundos entre disparos
      let newCooldown = 2.0
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
      
      -- 1. COMPORTAMIENTO BASADO EN TIPO DE ENEMIGO
      (vx_behavior, vy_behavior) = case enemyType enemy of
        Alien3 -> 
          -- Alien3 (Kamikaze): Siempre se dirige directo al jugador
          if dist_player > 0
            then (dx_player / dist_player, dy_player / dist_player)
            else (0, 0)
        
        Alien1 ->
          -- Alien1: Comportamiento táctico con rangos
          let maxRange = enemyShootRange enemy
              minRange = enemyMinRange enemy
              optimalRange = (maxRange + minRange) / 2
          in if dist_player > maxRange then
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
            -- En rango óptimo: orbitar
            if dist_player > 0
              then 
                let perpX = -dy_player / dist_player
                    perpY = dx_player / dist_player
                    towardsOptimal = (optimalRange - dist_player) / optimalRange
                    adjustX = (dx_player / dist_player) * towardsOptimal * 0.3
                    adjustY = (dy_player / dist_player) * towardsOptimal * 0.3
                in (perpX * 0.7 + adjustX, perpY * 0.7 + adjustY)
              else (0, 0)
        
        Alien2 ->
          -- Alien2: Comportamiento táctico similar a Alien1 (mismo patrón)
          let maxRange = enemyShootRange enemy
              minRange = enemyMinRange enemy
              optimalRange = (maxRange + minRange) / 2
          in if dist_player > maxRange then
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
            -- En rango óptimo: orbitar
            if dist_player > 0
              then 
                let perpX = -dy_player / dist_player
                    perpY = dx_player / dist_player
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