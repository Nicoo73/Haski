module Update
  ( updateWorld
  ) where

import GameState
import Enemy
import Wave
import Input (updateGameState, dirToVector)
import Item 
import Data.List (partition)

-------------------------------------------------------------
-- LÓGICA DE ACTUALIZACIÓN DEL MUNDO
-------------------------------------------------------------

updateWorld :: Float -> GameState -> GameState
updateWorld dt gs =
  let
    -- 1. ACTUALIZA EL JUGADOR
    gsPlayerMoved = updateGameState dt gs
    
    -- 2. SPAWN DE ENEMIGOS
    (spawned, newWave) = spawnWaveIfNeeded dt (wave gsPlayerMoved)
    
    -- 3. MOVIMIENTO DE ENEMIGOS
    allEnemies = enemies gsPlayerMoved ++ spawned
    movedEnemies = map (updateEnemy dt (playerPos gsPlayerMoved) allEnemies) allEnemies
    
    -- 4. MOVIMIENTO DE PROYECTILES
    movedBullets = updateBullets dt gsPlayerMoved
    
    -- 5. COLISIONES (Balas vs Enemigos) -> Genera Ítems
    (finalBullets, finalEnemies, droppedItems) = checkCollisions movedBullets movedEnemies

    -- 6. RECOLECCIÓN DE ITEMS (Jugador vs Items)
    currentItems = items gsPlayerMoved ++ droppedItems
    (remainingItems, gsItemsApplied) = checkItemCollection (playerPos gsPlayerMoved) currentItems gsPlayerMoved

    -- 7. LÓGICA DE AVANCE DE OLEADA
    isWaveFinished = null finalEnemies && enemiesLeft newWave == 0
    
    (newWaveState, newWaveCount) = if isWaveFinished
                                       then (nextWave (waveCount gs + 1), waveCount gs + 1)
                                       else (newWave, waveCount gs)
    
    -- 8. DEVOLVER EL ESTADO FINAL
  in gsItemsApplied { 
       enemies = finalEnemies,
       bullets = finalBullets,
       items   = remainingItems, 
       wave    = newWaveState,
       waveCount = newWaveCount
     }

-------------------------------------------------------------
-- COLISIONES: BALAS vs ENEMIGOS (Retorna items droppeados)
-------------------------------------------------------------

checkCollisions :: [Bullet] -> [Enemy] -> ([Bullet], [Enemy], [Item])
checkCollisions bullets enemiesList =
  let
    collisionDistSq = (3.0 + 12.0) ^ 2 

    isColliding bullet enemy =
      let (bx, by) = bulletPos bullet
          (ex, ey) = enemyPos enemy
          distSq = (bx - ex)^2 + (by - ey)^2
      in distSq < collisionDistSq

    (survivingBullets, survivingEnemies, newItems) = foldr (\bullet (bs, es, accItems) ->
        let (hits, misses) = partition (isColliding bullet) es
            -- Por cada enemigo muerto, intentamos crear un item
            itemsFromHits = [ item | enemy <- hits, Just item <- [spawnItemPure (enemyPos enemy)] ]
        in if null hits
           then (bullet : bs, es, accItems) 
           else (bs, misses, itemsFromHits ++ accItems) 
      ) ([], enemiesList, []) bullets
      
  in (survivingBullets, survivingEnemies, newItems)

-------------------------------------------------------------
-- RECOLECCIÓN DE ITEMS: JUGADOR vs ITEMS
-------------------------------------------------------------

checkItemCollection :: (Float, Float) -> [Item] -> GameState -> ([Item], GameState)
checkItemCollection (px, py) currentItems gs =
  let
    playerRadius = 16.0 
    
    isTouching item =
       let (ix, iy) = itemPos item
           r = itemRadius item
           distSq = (px - ix)^2 + (py - iy)^2
           minDist = playerRadius + r
       in distSq < minDist * minDist

    (collected, remaining) = partition isTouching currentItems
    
    -- Aplicar efectos de todos los items recogidos
    gsAfterEffects = foldr applyItemEffect gs collected
    
  in (remaining, gsAfterEffects)

-- Aplica el efecto de un solo item al GameState
applyItemEffect :: Item -> GameState -> GameState
applyItemEffect item gs = case itemType item of
  HealSmall   -> gs { playerHP = min 200 (playerHP gs + 10) }
  SpeedBoost  -> gs { currentSpeed = min 400.0 (currentSpeed gs + 20.0) }
  DamageBoost -> gs { playerDamage = playerDamage gs + 5 } -- Aumenta el daño en 5

-------------------------------------------------------------
-- LÓGICA DE MOVIMIENTO DE PROYECTILES
-------------------------------------------------------------

updateBullets :: Float -> GameState -> [Bullet]
updateBullets dt gs@GameState{ bullets = bs, windowSize = (winW, winH) } =
  let
    mapX = fromIntegral winW / 2
    mapY = fromIntegral winH / 2

    -- Patrón modificado para ignorar bulletDamage en el movimiento
    moveBullet b@Bullet{ bulletPos = (px1, py1), bulletDir = dir, bulletSpeed = speed } =
      let (vx, vy) = dirToVector dir
          newX = px1 + vx * speed * dt
          newY = py1 + vy * speed * dt
      in b { bulletPos = (newX, newY) }

    isOffScreen b =
      let (x, y) = bulletPos b
      in x < -mapX || x > mapX || y < -mapY || y > mapY
      
    movedBullets = map moveBullet bs
  in filter (not . isOffScreen) movedBullets