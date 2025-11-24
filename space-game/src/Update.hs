module Update
  ( updateWorld
  ) where

import GameState
import Enemy
import Wave
import Input (updateGameState, dirToVector)
import Data.List (partition)

-- (updateBullets y checkCollisions permanecen iguales, pero se usan aquí)

updateWorld :: Float -> GameState -> GameState
updateWorld dt gs =
  let
    -- 1. ACTUALIZA EL JUGADOR: Mueve al jugador y actualiza el tiempo de animación.
    gsPlayerMoved = updateGameState dt gs
    
    -- 2. MOVIMIENTO ENEMIGO Y OLEADAS: Genera enemigos y mueve a todos los existentes.
    (spawned, newWave) = spawnWaveIfNeeded dt (wave gsPlayerMoved)
    
    -- Enemigos existentes + Nuevos enemigos.
    allEnemies = enemies gsPlayerMoved ++ spawned
    
    -- Mueve a todos los enemigos hacia la posición actualizada del jugador.
    movedEnemies = map (updateEnemy dt (playerPos gsPlayerMoved) allEnemies) allEnemies
    
    -- 3. MOVIMIENTO DE PROYECTILES: Mueve las balas y limpia las que salen de pantalla.
    movedBullets = updateBullets dt gsPlayerMoved
    
    -- 4. COLISIONES: Revisa colisiones entre proyectiles movidos y enemigos movidos.
    (finalBullets, finalEnemies) = checkCollisions movedBullets movedEnemies
    
    -- 5. DEVOLVER EL ESTADO FINAL
  in gsPlayerMoved { 
       enemies = finalEnemies,
       bullets = finalBullets,
       wave    = newWave
     }

-------------------------------------------------------------
-- LÓGICA DE MOVIMIENTO DE PROYECTILES (Mismas funciones que antes, solo las incluyo para que sean completas)
-------------------------------------------------------------
-- Deben estar definidas aquí o ser importadas.
-- Si ya están definidas, no las necesitas pegar de nuevo, solo asegúrate que la función moveBullet usa el campo 'bulletSpeed'.

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