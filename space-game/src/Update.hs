module Update
  ( updateWorld
  ) where

import GameState
import Enemy
import Wave
import Input (updateGameState, dirToVector)
import Item 
import Data.List (partition)
import System.IO.Unsafe (unsafePerformIO)

-------------------------------------------------------------
-- LÓGICA PRINCIPAL
-------------------------------------------------------------

updateWorld :: Float -> GameState -> GameState
updateWorld dt gs =
  let
    -- 1. Mover Jugador
    gsPlayerMoved = updateGameState dt gs
    
    -- 2. Spawn Enemigos
    (spawned, newWave) = spawnWaveIfNeeded dt (wave gsPlayerMoved)
    
    -- 3. Mover Enemigos
    allEnemies = enemies gsPlayerMoved ++ spawned
    movedEnemies = map (updateEnemy dt (playerPos gsPlayerMoved) allEnemies) allEnemies
    
    -- 4. Disparo Enemigos
    (updatedEnemies, newEnemyBullets) = processEnemyShooting dt (playerPos gsPlayerMoved) movedEnemies
    allEnemyBullets = enemyBullets gsPlayerMoved ++ newEnemyBullets
    
    -- 5. Mover Balas (Jugador y Enemigo)
    movedBullets = updateBullets dt gsPlayerMoved
    movedEnemyBullets = updateEnemyBullets dt allEnemyBullets
    
    -- 5b. COLISIÓN KAMIKAZE: Alien3 explota al estar cerca del jugador
    (kamikazeEnemies, kamikazeDamage, kamikazeItems) = checkKamikazeCollisions (playerPos gsPlayerMoved) updatedEnemies
    
    -- 6. Colisiones: Bala Jugador vs Enemigo (Dropea Items)
    (finalBullets, finalEnemies, droppedItems) = checkCollisions movedBullets kamikazeEnemies
    
    -- 7. Colisiones: Bala Enemigo vs Jugador (Calculamos daño)
    (finalEnemyBullets, damageToPlayer) = checkPlayerCollisions movedEnemyBullets (playerPos gsPlayerMoved)
    
    -- Vida temporal después del daño (incluyendo kamikaze)
    totalDamage = damageToPlayer + kamikazeDamage
    healthAfterDamage = max 0 (currentHealth gsPlayerMoved - totalDamage)

    -- 8. Recolección de Items (Curación y Buffs)
    -- Creamos un estado base que YA TIENE el daño aplicado
    gsWithDamage = gsPlayerMoved { currentHealth = healthAfterDamage }
    
    -- Incluir items de balas y de kamikazes
    currentItems = items gsPlayerMoved ++ droppedItems ++ kamikazeItems
    
    -- checkItemCollection toma 'gsWithDamage' y le suma la curación si recoge ítems
    -- El resultado 'gsItemsApplied' tiene la vida final correcta (Vida - Daño + Curación)
    (remainingItems, gsItemsApplied) = checkItemCollection (playerPos gsPlayerMoved) currentItems gsWithDamage

    -- 9. Avanzar Oleada
    isWaveFinished = null finalEnemies && enemiesLeft newWave == 0
    
    (newWaveState, newWaveCount) = if isWaveFinished
                                       then (nextWave (waveCount gsPlayerMoved + 1), waveCount gsPlayerMoved + 1)
                                       else (newWave, waveCount gsPlayerMoved)
    
  -- 10. DEVOLVER EL ESTADO FINAL
  -- Usamos gsItemsApplied porque contiene la vida actualizada correctamente.
  -- SOLO actualizamos los campos que cambiaron independientemente (enemigos, balas, oleada).
  in gsItemsApplied { 
       enemies = finalEnemies,
       bullets = finalBullets,
       enemyBullets = finalEnemyBullets,
       items   = remainingItems,
       wave    = newWaveState,
       waveCount = newWaveCount
       -- ¡IMPORTANTE! NO agregues 'currentHealth = ...' aquí, o borrarás la curación.
     }

-------------------------------------------------------------
-- AUXILIARES (Movimiento y Colisiones)
-------------------------------------------------------------

updateBullets :: Float -> GameState -> [Bullet]
updateBullets dt gs@GameState{ bullets = bs, windowSize = (winW, winH) } =
  let mapX = fromIntegral winW / 2; mapY = fromIntegral winH / 2
      moveBullet b@Bullet{ bulletPos = (px, py), bulletDir = dir, bulletSpeed = speed } =
        let (vx, vy) = dirToVector dir
        in b { bulletPos = (px + vx * speed * dt, py + vy * speed * dt) }
      isOffScreen b = let (x, y) = bulletPos b in x < -mapX || x > mapX || y < -mapY || y > mapY
  in filter (not . isOffScreen) (map moveBullet bs)

updateEnemyBullets :: Float -> [EnemyBullet] -> [EnemyBullet]
updateEnemyBullets dt bullets =
  let terrainW = 640.0 
      terrainH = 360.0
      mapX = terrainW / 2
      mapY = terrainH / 2
      
      moveEnemyBullet eb@EnemyBullet{ eBulletPos = (px, py), eBulletDir = dir, eBulletSpeed = speed } =
        let (vx, vy) = dirToVector dir
            newX = px + vx * speed * dt
            newY = py + vy * speed * dt
        in eb { eBulletPos = (newX, newY) }
      
      isOffScreen eb =
        let (x, y) = eBulletPos eb
        in x < -mapX || x > mapX || y < -mapY || y > mapY
      
  in filter (not . isOffScreen) (map moveEnemyBullet bullets)

-- PROCESAR DISPARO ENEMIGO
processEnemyShooting :: Float -> (Float, Float) -> [Enemy] -> ([Enemy], [EnemyBullet])
processEnemyShooting dt playerPos enemies =
  let results = map (processEnemyShot dt playerPos) enemies
  in (map fst results, concatMap snd results)

processEnemyShot :: Float -> (Float, Float) -> Enemy -> (Enemy, [EnemyBullet])
processEnemyShot dt (px, py) enemy =
  let (ex, ey) = enemyPos enemy
      dx = px - ex; dy = py - ey; dist = sqrt (dx*dx + dy*dy)
  in if dist <= enemyShootRange enemy
     then let (shouldShoot, updatedEnemy) = unsafePerformIO (shouldEnemyShoot dt enemy)
          in if shouldShoot
             then (updatedEnemy, [EnemyBullet (enemyPos enemy) (vectorToDirection (dx/dist, dy/dist)) (enemyBulletSpd enemy) (enemyDamage enemy)])
             else (updatedEnemy, [])
     else (enemy, [])

-- COLISIONES
checkCollisions :: [Bullet] -> [Enemy] -> ([Bullet], [Enemy], [Item])
checkCollisions bullets enemies =
  let collisionDistSq = (3.0 + 12.0) ^ 2 
      isColliding b e = let (bx,by) = bulletPos b; (ex,ey) = enemyPos e in (bx-ex)^2 + (by-ey)^2 < collisionDistSq
      
      -- Procesar cada bala y actualizar enemigos
      (survivingBullets, survivingEnemies, droppedItems) = foldr (\b (bs, es, accI) ->
          let (hits, misses) = Data.List.partition (isColliding b) es
          in if null hits 
             then (b:bs, es, accI)  -- Bala no golpeó nada
             else 
               -- Bala golpeó enemigo(s), restar vida
               let damage = bulletDamage b
                   -- Aplicar daño al primer enemigo golpeado
                   damagedEnemy = head hits
                   newHealth = enemyHealth damagedEnemy - damage
                   
                   -- Si el enemigo muere, generar item y eliminarlo
                   (finalEnemies, newItems) = 
                     if newHealth <= 0
                     then (tail hits ++ misses, 
                           case spawnItemPure (enemyPos damagedEnemy) of
                             Just item -> [item]
                             Nothing -> [])
                     else (damagedEnemy { enemyHealth = newHealth } : tail hits ++ misses, [])
                   
               in (bs, finalEnemies, accI ++ newItems)  -- Bala se consume
        ) ([], enemies, []) bullets
  in (survivingBullets, survivingEnemies, droppedItems)

checkPlayerCollisions :: [EnemyBullet] -> (Float, Float) -> ([EnemyBullet], Int)
checkPlayerCollisions ebs (px, py) =
  let colDistSq = (8.0 + 3.0) ^ 2
      isHit eb = let (bx,by) = eBulletPos eb in (bx-px)^2 + (by-py)^2 < colDistSq
      (hits, misses) = Data.List.partition isHit ebs
  in (misses, sum (map eBulletDamage hits))

-- COLISIÓN KAMIKAZE: Alien3 explota al estar cerca del jugador
checkKamikazeCollisions :: (Float, Float) -> [Enemy] -> ([Enemy], Int, [Item])
checkKamikazeCollisions (px, py) enemies =
  let kamikazeDistSq = 16.0 ^ 2  -- Explota a 16px de distancia
      isKamikaze e = enemyType e == Alien3
      distToPlayerSq e = let (ex, ey) = enemyPos e in (px - ex)^2 + (py - ey)^2
      
      -- Separar kamikazes que están cerca del jugador
      (kamikazes, others) = partition isKamikaze enemies
      (exploding, surviving) = partition (\e -> distToPlayerSq e < kamikazeDistSq) kamikazes
      
      -- Calcular daño total de las explosiones
      totalDamage = sum (map enemyDamage exploding)
      
      -- Generar items de los Alien3 que explotaron
      droppedItems = [ item | enemy <- exploding, Just item <- [spawnItemPure (enemyPos enemy)] ]
      
      -- Enemigos que sobreviven (los que no explotaron)
      survivingEnemies = surviving ++ others
      
  in (survivingEnemies, totalDamage, droppedItems)

checkItemCollection :: (Float, Float) -> [Item] -> GameState -> ([Item], GameState)
checkItemCollection (px, py) items gs =
  let distSq i = let (ix,iy) = itemPos i in (px-ix)^2 + (py-iy)^2
      minDistSq i = (playerPickupRange + itemRadius i) ^ 2
      (collected, remaining) = partition (\i -> distSq i < minDistSq i) items
      gsNew = foldr applyItemEffect gs collected
  in (remaining, gsNew)

-- APLICAR EFECTO DEL ÍTEM
applyItemEffect :: Item -> GameState -> GameState
applyItemEffect item gs@GameState{ currentStats = cs } = 
  case itemType item of
    -- Cura 20 de vida, respetando el máximo definido en playerHealth de los stats
    HealSmall   -> gs { currentHealth = min (playerHealth cs) (currentHealth gs + 20) }
    SpeedBoost  -> gs { currentStats = cs { playerSpeedBonus = playerSpeedBonus cs + 25.0 } } 
    DamageBoost -> gs { currentStats = cs { playerDamageBonus = playerDamageBonus cs + 5 } }