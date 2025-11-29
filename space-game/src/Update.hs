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
import qualified Boss as B


-------------------------------------------------------------
-- LÓGICA PRINCIPAL
-------------------------------------------------------------

updateWorld :: Float -> GameState -> GameState
updateWorld dt gs =
  let
    -- 1. Mover Jugador y proyectiles básicos
    gsPlayerMoved = updateGameState dt gs
    
    -- 2. GESTIÓN DEL BOSS Y OLEADAS
    -- Si ya pasamos la oleada 3 y no hay enemigos, intentamos spawnear al Boss
    shouldSpawnBoss = waveCount gsPlayerMoved > 3 && 
                      null (enemies gsPlayerMoved) && 
                      not (bossSpawned gsPlayerMoved)
    
    gsBossLogic = if shouldSpawnBoss
                  then if bossSpawnTimer gsPlayerMoved >= 3.0 -- Delay de 3 segundos
                       then gsPlayerMoved { maybeBoss = Just B.createBoss, bossSpawned = True }
                       else gsPlayerMoved { bossSpawnTimer = bossSpawnTimer gsPlayerMoved + dt }
                  else gsPlayerMoved
    
    -- CORRECCIÓN AQUÍ: Solo spawnear oleadas normales si NO ha spawneado el boss Y estamos en oleada <= 3
    (spawned, newWave) = if not (bossSpawned gsBossLogic) && waveCount gsBossLogic <= 3
                         then spawnWaveIfNeeded dt (wave gsBossLogic) (waveCount gsBossLogic)
                         else ([], wave gsBossLogic)

    -- 3. Mover Enemigos Normales
    allEnemies = enemies gsBossLogic ++ spawned
    movedEnemies = map (updateEnemy dt (playerPos gsBossLogic) allEnemies) allEnemies
    
    -- 4. Disparo Enemigos Normales
    (updatedEnemies, newEnemyBullets) = processEnemyShooting dt (playerPos gsBossLogic) movedEnemies
    allEnemyBullets = enemyBullets gsBossLogic ++ newEnemyBullets
    
    -- 5. LÓGICA DEL BOSS (Movimiento y Ataques)
    (gsWithBossUpdate, bossNewAttacks) = case maybeBoss gsBossLogic of
        Nothing -> (gsBossLogic, [])
        Just b -> 
            let (updatedBoss, attacks) = B.updateBoss dt (playerPos gsBossLogic) b
            in (gsBossLogic { maybeBoss = Just updatedBoss }, attacks)
            
    -- Convertir ataques del boss a EnemyBullets (simplificado para colisiones)
    bossBulletProjectiles = concatMap (bossAttackToBullets dt) bossNewAttacks

    -- 6. Mover Balas (Jugador, Enemigos y Boss)
    movedBullets = updateBullets dt gsWithBossUpdate
    movedAllEnemyBullets = updateEnemyBullets dt (allEnemyBullets ++ bossBulletProjectiles)
    
    -- 7. Colisiones: 
    -- 7a. Kamikaze
    (kamikazeEnemies, kamikazeDamage, kamikazeItems) = checkKamikazeCollisions (playerPos gsWithBossUpdate) updatedEnemies
    
    -- 7b. Bala Jugador vs Enemigos Normales
    (finalBullets, finalEnemies, droppedItems) = checkCollisions movedBullets kamikazeEnemies
    
    -- 7c. Bala Jugador vs BOSS
    (bulletsAfterBoss, finalBossState, bossDied) = checkBossCollisions finalBullets (maybeBoss gsWithBossUpdate)

    -- 8. Colisiones: Bala Enemiga/Boss vs Jugador
    (finalEnemyBullets, damageToPlayer) = checkPlayerCollisions movedAllEnemyBullets (playerPos gsWithBossUpdate)
    
    -- 9. Verificar Vida del Jugador
    totalDamage = damageToPlayer + kamikazeDamage
    healthAfterDamage = max 0 (currentHealth gsWithBossUpdate - totalDamage)

    in 
     -- CONDICIÓN DE VICTORIA: Si el Boss murió
     if bossDied 
     then gs { currentScreen = Victory }
     -- CONDICIÓN DE DERROTA
     else if healthAfterDamage <= 0
     then gs { currentScreen = GameOver, currentHealth = 0 }
     else
        let
            -- Actualizar estado final
            gsWithDamage = gsWithBossUpdate { currentHealth = healthAfterDamage, maybeBoss = finalBossState }
            
            -- Recoger items
            currentItems = items gsPlayerMoved ++ droppedItems ++ kamikazeItems
            (remainingItems, gsItemsApplied) = checkItemCollection (playerPos gsPlayerMoved) currentItems gsWithDamage
            
            -- Progresión de oleadas (solo si no hay boss y estamos en oleadas normales)
            isWaveFinished = null finalEnemies && enemiesLeft newWave == 0 && not (bossSpawned gsItemsApplied)
            (newWaveState, newWaveCount) = if isWaveFinished
                                            then (nextWave (waveCount gsPlayerMoved + 1), waveCount gsPlayerMoved + 1)
                                            else (newWave, waveCount gsPlayerMoved)
        in gsItemsApplied { 
             enemies = finalEnemies,
             bullets = bulletsAfterBoss, 
             enemyBullets = finalEnemyBullets,
             items   = remainingItems,
             wave    = newWaveState,
             waveCount = newWaveCount,
             bossAttacks = bossAttacks gsWithBossUpdate ++ bossNewAttacks 
           }

-------------------------------------------------------------
-- AUXILIARES
-------------------------------------------------------------

-- Convierte el ataque abstracto del Boss a balas físicas para colisión
bossAttackToBullets :: Float -> B.BossAttack -> [EnemyBullet]
bossAttackToBullets _ ba = 
    [ EnemyBullet 
      { eBulletPos = B.attackPos ba
      , eBulletDir = B.attackDir ba
      , eBulletSpeed = B.attackSpeed ba
      , eBulletDamage = B.attackDamage ba
      }
    ]

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

-- COLISIONES JUGADOR VS ENEMIGOS
checkCollisions :: [Bullet] -> [Enemy] -> ([Bullet], [Enemy], [Item])
checkCollisions bullets enemies =
  let collisionDistSq = (3.0 + 12.0) ^ 2 
      isColliding b e = let (bx,by) = bulletPos b; (ex,ey) = enemyPos e in (bx-ex)^2 + (by-ey)^2 < collisionDistSq
      
      (survivingBullets, survivingEnemies, droppedItems) = foldr (\b (bs, es, accI) ->
          let (hits, misses) = Data.List.partition (isColliding b) es
          in if null hits 
             then (b:bs, es, accI)
             else 
               let damage = bulletDamage b
                   damagedEnemy = head hits
                   newHealth = enemyHealth damagedEnemy - damage
                   
                   (finalEnemies, newItems) = 
                     if newHealth <= 0
                     then (tail hits ++ misses, 
                           case spawnItemPure (enemyPos damagedEnemy) of
                             Just item -> [item]
                             Nothing -> [])
                     else (damagedEnemy { enemyHealth = newHealth } : tail hits ++ misses, [])
                   
               in (bs, finalEnemies, accI ++ newItems)
        ) ([], enemies, []) bullets
  in (survivingBullets, survivingEnemies, droppedItems)

-- COLISIONES JUGADOR VS BOSS
checkBossCollisions :: [Bullet] -> Maybe B.Boss -> ([Bullet], Maybe B.Boss, Bool)
checkBossCollisions bullets Nothing = (bullets, Nothing, False)
checkBossCollisions bullets (Just boss) =
    let collisionDistSq = (3.0 + B.bossRadius) ^ 2 
        isColliding b = let (bx, by) = bulletPos b
                            (bossX, bossY) = B.bossPos boss
                        in (bx - bossX)^2 + (by - bossY)^2 < collisionDistSq
        
        (hits, misses) = Data.List.partition isColliding bullets
        totalDamage = sum (map bulletDamage hits)
        newHealth = B.bossHealth boss - totalDamage
        
    in if newHealth <= 0
       then (misses, Nothing, True) -- Boss muere
       else (misses, Just (boss { B.bossHealth = newHealth }), False)

-- COLISIONES ENEMIGOS VS JUGADOR
checkPlayerCollisions :: [EnemyBullet] -> (Float, Float) -> ([EnemyBullet], Int)
checkPlayerCollisions ebs (px, py) =
  let colDistSq = (8.0 + 3.0) ^ 2
      isHit eb = let (bx,by) = eBulletPos eb in (bx-px)^2 + (by-py)^2 < colDistSq
      (hits, misses) = Data.List.partition isHit ebs
  in (misses, sum (map eBulletDamage hits))

-- COLISIÓN KAMIKAZE
checkKamikazeCollisions :: (Float, Float) -> [Enemy] -> ([Enemy], Int, [Item])
checkKamikazeCollisions (px, py) enemies =
  let kamikazeDistSq = 16.0 ^ 2
      isKamikaze e = enemyType e == Alien3
      distToPlayerSq e = let (ex, ey) = enemyPos e in (px - ex)^2 + (py - ey)^2
      
      (kamikazes, others) = partition isKamikaze enemies
      (exploding, surviving) = partition (\e -> distToPlayerSq e < kamikazeDistSq) kamikazes
      
      totalDamage = sum (map enemyDamage exploding)
      droppedItems = [ item | enemy <- exploding, Just item <- [spawnItemPure (enemyPos enemy)] ]
      survivingEnemies = surviving ++ others
      
  in (survivingEnemies, totalDamage, droppedItems)

checkItemCollection :: (Float, Float) -> [Item] -> GameState -> ([Item], GameState)
checkItemCollection (px, py) items gs =
  let distSq i = let (ix,iy) = itemPos i in (px-ix)^2 + (py-iy)^2
      minDistSq i = (playerPickupRange + itemRadius i) ^ 2
      (collected, remaining) = partition (\i -> distSq i < minDistSq i) items
      gsNew = foldr applyItemEffect gs collected
  in (remaining, gsNew)

applyItemEffect :: Item -> GameState -> GameState
applyItemEffect item gs@GameState{ currentStats = cs } = 
  case itemType item of
    HealSmall ->
      gs { currentHealth = min (playerHealth cs) (currentHealth gs + 20) }

    SpeedBoost ->
      let newSpeedBonus = playerSpeedBonus cs + 25.0
          newMoveSpeed  = playerMoveSpeed cs + 25.0
      in gs { currentStats = cs { playerSpeedBonus = newSpeedBonus
                                , playerMoveSpeed  = newMoveSpeed } }

    DamageBoost ->
      let newDamageBonus = playerDamageBonus cs + 5
          newDamage      = playerDamage cs + 5
      in gs { currentStats = cs { playerDamageBonus = newDamageBonus
                                , playerDamage     = newDamage } }