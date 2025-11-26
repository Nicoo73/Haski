module Update
  ( updateWorld
  ) where

import GameState
import Enemy
import Wave
import Input (updateGameState, dirToVector)
import Item -- IMPORTADO: Para usar el módulo Item
import Data.List (partition)
import System.Random (randomRIO)
import System.IO.Unsafe (unsafePerformIO)  -- Para IO en contexto puro

-------------------------------------------------------------
-- LÓGICA DE ACTUALIZACIÓN DEL MUNDO (MODIFICADO)
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
    
    -- 3.5. DISPARO DE ENEMIGOS: Procesa qué enemigos disparan
    (updatedEnemies, newEnemyBullets) = processEnemyShooting dt (playerPos gsPlayerMoved) movedEnemies
    allEnemyBullets = enemyBullets gsPlayerMoved ++ newEnemyBullets
    
    -- 4. MOVIMIENTO DE PROYECTILES: Mueve las balas y limpia las que salen.
    movedBullets = updateBullets dt gsPlayerMoved
    movedEnemyBullets = updateEnemyBullets dt allEnemyBullets
    
    -- 5. COLISIONES: Revisa colisiones entre proyectiles y enemigos.
    -- NUEVO: checkCollisions ahora devuelve los ítems dropeados.
    (finalBullets, finalEnemies, droppedItems) = checkCollisions movedBullets updatedEnemies
    
    -- 5.5. COLISIONES CON JUGADOR: Revisa colisiones entre proyectiles enemigos y jugador
    (finalEnemyBullets, damageToPlayer) = checkPlayerCollisions movedEnemyBullets (playerPos gsPlayerMoved)
    newPlayerHealth = max 0 (currentHealth gsPlayerMoved - damageToPlayer)

    -- 5.6. RECOLECCIÓN DE ITEMS (Jugador vs Items) -- NUEVO
    currentItems = items gsPlayerMoved ++ droppedItems
    (remainingItems, gsItemsApplied) = checkItemCollection (playerPos gsPlayerMoved) currentItems gsPlayerMoved

    -- 6. LÓGICA DE AVANCE DE OLEADA: Verifica si la oleada terminó.
    isWaveFinished = null finalEnemies && enemiesLeft newWave == 0
    
    -- 7. CALCULAR LA PRÓXIMA OLEADA
    (newWaveState, newWaveCount) = if isWaveFinished
                                       then (nextWave (waveCount gsPlayerMoved + 1), waveCount gsPlayerMoved + 1)
                                       else (newWave, waveCount gsPlayerMoved)
    
    -- 8. DEVOLVER EL ESTADO FINAL
    -- Usar gsItemsApplied que ya tiene los nuevos currentStats
  in gsItemsApplied { 
       enemies = finalEnemies,
       bullets = finalBullets,
       enemyBullets = finalEnemyBullets,
       items   = remainingItems, -- Ítems restantes en el mapa
       wave    = newWaveState,
       waveCount = newWaveCount,
       currentHealth = newPlayerHealth
     }

-------------------------------------------------------------
-- LÓGICA DE MOVIMIENTO DE PROYECTILES (Mismas funciones que antes)
-------------------------------------------------------------
-- ... (updateBullets y updateEnemyBullets permanecen iguales) ...

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

-------------------------------------------------------------
-- LÓGICA DE DISPARO DE ENEMIGOS
-------------------------------------------------------------
-- ... (processEnemyShooting y processEnemyShot permanecen iguales) ...

-- Procesa los disparos de enemigos
processEnemyShooting :: Float -> (Float, Float) -> [Enemy] -> ([Enemy], [EnemyBullet])
processEnemyShooting dt playerPos enemies =
  let results = map (processEnemyShot dt playerPos) enemies
      updatedEnemies = map fst results
      allNewBullets = concatMap snd results
  in (updatedEnemies, allNewBullets)

-- Procesa el disparo de un enemigo individual
processEnemyShot :: Float -> (Float, Float) -> Enemy -> (Enemy, [EnemyBullet])
processEnemyShot dt (px, py) enemy =
  let (ex, ey) = enemyPos enemy
      dx = px - ex
      dy = py - ey
      distToPlayer = sqrt (dx*dx + dy*dy)
      
      -- Solo dispara si está en rango
      inRange = distToPlayer <= enemyShootRange enemy
      
  in if inRange
     then
       -- Usar unsafePerformIO para manejar IO en contexto puro
       let (shouldShoot, updatedEnemy) = unsafePerformIO (shouldEnemyShoot dt enemy)
       in if shouldShoot
          then
            -- Calcular dirección hacia el jugador
            let shootDir = vectorToDirection (dx / distToPlayer, dy / distToPlayer)
                newBullet = EnemyBullet
                  { eBulletPos = enemyPos enemy
                  , eBulletDir = shootDir
                  , eBulletSpeed = enemyBulletSpd enemy
                  , eBulletDamage = enemyDamage enemy
                  }
            in (updatedEnemy, [newBullet])
          else (updatedEnemy, [])
     else (enemy, [])

-- Mueve las balas enemigas
updateEnemyBullets :: Float -> [EnemyBullet] -> [EnemyBullet]
updateEnemyBullets dt bullets =
  let terrainW = 640.0  -- Actualizado
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
      
      movedBullets = map moveEnemyBullet bullets
  in filter (not . isOffScreen) bullets


-------------------------------------------------------------
-- LÓGICA DE COLISIONES (MODIFICADA: Devuelve Items)
-------------------------------------------------------------

checkCollisions :: [Bullet] -> [Enemy] -> ([Bullet], [Enemy], [Item])
checkCollisions bullets enemies =
  let
    collisionDistSq = (3.0 + 12.0) ^ 2 

    isColliding bullet enemy =
      let (bx, by) = bulletPos bullet
          (ex, ey) = enemyPos enemy
          distSq = (bx - ex)^2 + (by - ey)^2
      in distSq < collisionDistSq

    -- Ahora, si una bala golpea, se desecha y el enemigo también (1 hit kill)
    (survivingBullets, survivingEnemies, droppedItems) = foldr (\bullet (bs, es, accItems) ->
        let (hits, misses) = Data.List.partition (isColliding bullet) es
            -- Por cada enemigo 'hit' se intenta generar un ítem
            newItems = [ item | enemy <- hits, Just item <- [spawnItemPure (enemyPos enemy)] ]
        in if null hits
           then (bullet : bs, es, accItems)
           else (bs, misses, accItems ++ newItems) -- La bala desaparece, el enemigo desaparece
      ) ([], enemies, []) bullets
      
  in (survivingBullets, survivingEnemies, droppedItems)

-------------------------------------------------------------
-- LÓGICA DE COLISIONES CON EL JUGADOR
-------------------------------------------------------------
-- ... (checkPlayerCollisions permanece igual) ...

-- Verifica colisiones entre balas enemigas y el jugador
-- Retorna las balas que no colisionaron y el daño total recibido
checkPlayerCollisions :: [EnemyBullet] -> (Float, Float) -> ([EnemyBullet], Int)
checkPlayerCollisions enemyBullets playerPos =
  let
    playerRadius = 8.0  -- Radio del jugador (mitad de 16px)
    bulletRadius = 3.0  -- Radio de la bala enemiga
    collisionDistSq = (playerRadius + bulletRadius) ^ 2
    
    (px, py) = playerPos
    
    isCollidingWithPlayer eBullet =
      let (bx, by) = eBulletPos eBullet
          distSq = (bx - px)^2 + (by - py)^2
      in distSq < collisionDistSq
    
    -- Separar balas que colisionan de las que no
    (hits, misses) = Data.List.partition isCollidingWithPlayer enemyBullets
    
    -- Calcular daño total de todas las balas que impactaron
    totalDamage = sum (map eBulletDamage hits)
    
  in (misses, totalDamage)

-------------------------------------------------------------
-- RECOLECCIÓN DE ITEMS: JUGADOR vs ITEMS (NUEVO)
-------------------------------------------------------------

checkItemCollection :: (Float, Float) -> [Item] -> GameState -> ([Item], GameState)
checkItemCollection (px, py) currentItems gs =
  let
    -- Usamos el RANGO DE RECOLECCIÓN, que es más grande que el radio físico.
    pickupRadius = playerPickupRange 
    
    isTouching item =
       let (ix, iy) = itemPos item
           r = itemRadius item
           dx = px - ix
           dy = py - iy
           distSq = dx*dx + dy*dy                 -- Distancia al cuadrado
           minDist = pickupRadius + r             -- Se usa pickupRadius aquí
       in distSq < minDist * minDist               
    
    (collected, remaining) = partition isTouching currentItems
    
    -- Aplicar efectos de todos los items recogidos
    gsAfterEffects = foldr applyItemEffect gs collected
    
  in (remaining, gsAfterEffects)

-- Aplica el efecto de un solo item al GameState
applyItemEffect :: Item -> GameState -> GameState
applyItemEffect item gs@GameState{ currentStats = cs } = 
  case itemType item of
    HealSmall   -> gs { currentHealth = min (playerHealth cs) (currentHealth gs + 2) } -- Curar 2 HP
    SpeedBoost  -> gs { currentStats = cs { playerSpeedBonus = playerSpeedBonus cs + 25.0 } } -- +25 velocidad
    DamageBoost -> gs { currentStats = cs { playerDamageBonus = playerDamageBonus cs + 1 } } -- +1 daño