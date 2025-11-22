module Enemy
  ( Enemy(..)
  , updateEnemy
  , enemySpeed
  ) where
    
-- Datos de un enemigo
data Enemy = Enemy
  { enemyPos   :: (Float, Float)
  , enemySpeed :: Float
  } deriving (Show)

-- Actualiza la posición del enemigo moviéndose hacia el jugador
updateEnemy :: Float -> (Float, Float) -> Enemy -> Enemy
updateEnemy dt (px, py) enemy =
  let (ex, ey) = enemyPos enemy
      dx = px - ex
      dy = py - ey
      dist = sqrt (dx*dx + dy*dy)
      (vx, vy) = if dist > 0
                 then (dx / dist, dy / dist)
                 else (0,0)
      speed = enemySpeed enemy
  in enemy { enemyPos = (ex + vx * speed * dt, ey + vy * speed * dt) }
