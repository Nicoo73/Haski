module Update
  ( updateWorld
  ) where

import GameState
import Enemy
import Wave
import Input (updateGameState, dirToVector)
import Data.List (partition)

-------------------------------------------------------------
-- LÓGICA DE ACTUALIZACIÓN DEL MUNDO
-------------------------------------------------------------

updateWorld :: Float -> GameState -> GameState
updateWorld dt gs =
  let
    -- 1. ACTUALIZA EL JUGADOR: Mueve al jugador y actualiza el tiempo.
    gsPlayerMoved = updateGameState dt gs
    
    -- 2. SPAWN DE ENEMIGOS: Genera enemigos (actualiza el contador de la oleada)
    (spawned, newWave) = spawnWaveIfNeeded dt (wave gsPlayerMoved)
    
    -- 3. MOVIMIENTO DE ENEMIGOS: Prepara la lista completa y los mueve (incluyendo lógica de Separación)
    allEnemies = enemies gsPlayerMoved ++ spawned
    movedEnemies = map (updateEnemy dt (playerPos gsPlayerMoved) allEnemies) allEnemies
    
    -- 4. MOVIMIENTO DE PROYECTILES: Mueve las balas y limpia las que salen.
    movedBullets = updateBullets dt gsPlayerMoved
    
    -- 5. COLISIONES: Revisa colisiones entre proyectiles y enemigos.
    (finalBullets, finalEnemies) = checkCollisions movedBullets movedEnemies

    -- 6. LÓGICA DE AVANCE DE OLEADA: Verifica si la oleada terminó.
    -- El estado actual de la oleada es 'newWave' (el resultado de spawnWaveIfNeeded)
    isWaveFinished = null finalEnemies && enemiesLeft newWave == 0 -- ¡CORRECCIÓN AQUÍ!
    
    -- 7. CALCULAR LA PRÓXIMA OLEADA
    (newWaveState, newWaveCount) = if isWaveFinished
                                       -- Si terminó, crea la siguiente oleada (waveCount + 1)
                                       then (nextWave (waveCount gsPlayerMoved + 1), waveCount gsPlayerMoved + 1)
                                       -- Si no terminó, mantiene el estado de la oleada (newWave) y el contador
                                       else (newWave, waveCount gsPlayerMoved) -- ¡CORRECCIÓN AQUÍ!
    
    -- 8. DEVOLVER EL ESTADO FINAL
  in gsPlayerMoved { 
       enemies = finalEnemies,
       bullets = finalBullets,
       wave    = newWaveState,
       waveCount = newWaveCount
     }

-------------------------------------------------------------
-- LÓGICA DE MOVIMIENTO DE PROYECTILES (Mismas funciones que antes)
-------------------------------------------------------------

updateBullets :: Float -> GameState -> [Bullet]
updateBullets dt gs@GameState{ bullets = bs, windowSize = (winW, winH) } =
  let
    mapX = fromIntegral winW / 2
    mapY = fromIntegral winH / 2

    moveBullet b@Bullet{ bulletPos = (px, py), bulletDir = dir, bulletSpeed = speed } =
      let (vx, vy) = dirToVector dir
          newX = px + vx * speed * dt
          newY = py + vy * speed * dt
      in b { bulletPos = (newX, newY) }

    isOffScreen b =
      let (x, y) = bulletPos b
      in x < -mapX || x > mapX || y < -mapY || y > mapY
      
    movedBullets = map moveBullet bs
  in filter (not . isOffScreen) movedBullets

checkCollisions :: [Bullet] -> [Enemy] -> ([Bullet], [Enemy])
checkCollisions bullets enemies =
  let
    collisionDistSq = (3.0 + 12.0) ^ 2 

    isColliding bullet enemy =
      let (bx, by) = bulletPos bullet
          (ex, ey) = enemyPos enemy
          distSq = (bx - ex)^2 + (by - ey)^2
      in distSq < collisionDistSq

    (survivingBullets, survivingEnemies) = foldr (\bullet (bs, es) ->
        let (hits, misses) = Data.List.partition (isColliding bullet) es
        in if null hits
           then (bullet : bs, es)
           else (bs, misses)
      ) ([], enemies) bullets
      
  in (survivingBullets, survivingEnemies)