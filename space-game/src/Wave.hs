module Wave
  ( spawnWaveIfNeeded
  , nextWave
  , enemiesForWave
  ) where

import Enemy
import GameState (PlayerStats(..), playerStats, Wave(..), playerRange, terrainWidth, terrainHeight)
import System.Random (randomRIO)
import System.IO.Unsafe (unsafePerformIO)

-------------------------------------------------------------
-- LÓGICA DE PROGRESIÓN DE OLEADAS
-------------------------------------------------------------

-- Define la cantidad de enemigos para una oleada específica (6 enemigos fijos: 4 Alien1 + 2 Alien3)
enemiesForWave :: Int -> Int
enemiesForWave _waveNum = 6  -- Siempre 6 enemigos por oleada


-- Crea un nuevo estado de oleada, listo para spawnear
nextWave :: Int -> Wave
nextWave nextWaveNum = Wave
  { enemiesLeft = enemiesForWave nextWaveNum
  , timeSinceSpawn = 0
  }

-------------------------------------------------------------
-- LÓGICA DE SPAWN
-------------------------------------------------------------

-- Genera una posición aleatoria en uno de los bordes del mapa
randomEdgePosition :: IO (Float, Float)
randomEdgePosition = do
  let halfW = terrainWidth / 2
      halfH = terrainHeight / 2
      margin = 16.0  -- Margen desde el borde
  
  -- Seleccionar un lado aleatorio: 0=arriba, 1=derecha, 2=abajo, 3=izquierda
  side <- randomRIO (0 :: Int, 3)
  
  case side of
    0 -> do -- Arriba
      x <- randomRIO (-halfW + margin, halfW - margin)
      return (x, halfH - margin)
    1 -> do -- Derecha
      y <- randomRIO (-halfH + margin, halfH - margin)
      return (halfW - margin, y)
    2 -> do -- Abajo
      x <- randomRIO (-halfW + margin, halfW - margin)
      return (x, -halfH + margin)
    _ -> do -- Izquierda
      y <- randomRIO (-halfH + margin, halfH - margin)
      return (-halfW + margin, y)

-- Genera un enemigo si el tiempo lo permite y quedan enemigos por spawnear
spawnWaveIfNeeded :: Float -> Wave -> ([Enemy], Wave)
spawnWaveIfNeeded dt wave =
  let newTime = timeSinceSpawn wave + dt
      totalInWave = enemiesForWave 1  -- 6 total
      spawnedCount = totalInWave - enemiesLeft wave  -- Cuántos ya se spawnearon
  in if enemiesLeft wave > 0 && newTime >= 1 -- Genera uno cada 1 segundo
       then 
         -- Generar posición aleatoria para CADA enemigo
         let (spawnPos, enemy) = unsafePerformIO $ do
               pos <- randomEdgePosition
               -- Primeros 2 son Alien1, siguientes 2 son Alien2, últimos 2 son Alien3
               let e = if spawnedCount < 2
                       then createAlien1 pos (playerMoveSpeed playerStats) (playerBulletSpeed playerStats) playerRange
                       else if spawnedCount < 4
                       then createAlien2 pos (playerMoveSpeed playerStats) (playerBulletSpeed playerStats) playerRange
                       else createAlien3 pos (playerMoveSpeed playerStats) (playerBulletSpeed playerStats) playerRange
               return (pos, e)
         in ([enemy], wave { enemiesLeft = enemiesLeft wave - 1, timeSinceSpawn = 0 })
       else ([], wave { timeSinceSpawn = newTime })