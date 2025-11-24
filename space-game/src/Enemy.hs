-- src/Enemy.hs

module Enemy
  ( Enemy(..)
  , updateEnemy
  , enemySpeed
  , enemyRadius
  ) where 
    
-- Datos de un enemigo
data Enemy = Enemy
  { enemyPos   :: (Float, Float)
  , enemySpeed :: Float
  } deriving (Show)

enemyRadius :: Float
enemyRadius = 12.0 -- Definir el radio para las colisiones (círculo sólido en Render.hs)

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

-- Actualiza la posición del enemigo moviéndose hacia el jugador (MODIFICADO)
updateEnemy :: Float -> (Float, Float) -> [Enemy] -> Enemy -> Enemy
updateEnemy dt (px, py) allEnemies enemy =
  let (ex, ey) = enemyPos enemy
      
      -- 1. FUERZA DE BÚSQUEDA (SEEK - hacia el jugador)
      dx_seek = px - ex
      dy_seek = py - ey
      dist_seek = sqrt (dx_seek*dx_seek + dy_seek*dy_seek)
      (vx_seek, vy_seek) = if dist_seek > 0
                           then (dx_seek / dist_seek, dy_seek / dist_seek)
                           else (0,0)
      
      -- 2. FUERZA DE SEPARACIÓN (SEPARATION - de otros enemigos)
      -- Filtra la lista para no incluirse a sí mismo
      otherEnemies = filter (\e -> enemyPos e /= enemyPos enemy) allEnemies
      (vx_sep, vy_sep) = calculateSeparationForce otherEnemies enemy
      
      -- 3. COMBINAR FUERZAS (Separación es más fuerte para evitar amontonamiento)
      -- Ajustar estos pesos (1.0 y 2.0) controla el comportamiento
      vx_final = (vx_seek * 1.0) + (vx_sep * 2.0)
      vy_final = (vy_seek * 1.0) + (vy_sep * 2.0)
      
      -- Normalizar el vector de movimiento final
      magnitude = sqrt (vx_final*vx_final + vy_final*vy_final)
      (vx_norm, vy_norm) = if magnitude > 0
                           then (vx_final / magnitude, vy_final / magnitude)
                           else (0, 0)
                           
      speed = enemySpeed enemy
  in enemy { enemyPos = (ex + vx_norm * speed * dt, ey + vy_norm * speed * dt) }