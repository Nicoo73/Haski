module Wave
  ( spawnWaveIfNeeded
  , nextWave
  , enemiesForWave
  ) where

import Enemy
import GameState (PlayerStats(..), playerStats, Wave(..), playerRange)

-------------------------------------------------------------
-- LÓGICA DE PROGRESIÓN DE OLEADAS
-------------------------------------------------------------

-- Define la cantidad de enemigos para una oleada específica (n + 5 por oleada extra)
enemiesForWave :: Int -> Int
enemiesForWave waveNum = 5 + (waveNum - 1) * 5


-- Crea un nuevo estado de oleada, listo para spawnear
nextWave :: Int -> Wave
nextWave nextWaveNum = Wave
  { enemiesLeft = enemiesForWave nextWaveNum
  , timeSinceSpawn = 0
  }

-------------------------------------------------------------
-- LÓGICA DE SPAWN
-------------------------------------------------------------

-- Genera un enemigo si el tiempo lo permite y quedan enemigos por spawnear
spawnWaveIfNeeded :: Float -> Wave -> ([Enemy], Wave)
spawnWaveIfNeeded dt wave =
  let newTime = timeSinceSpawn wave + dt
  in if enemiesLeft wave > 0 && newTime >= 1 -- Genera uno cada 1 segundo
       then let enemy = createCaza (0, 200) (playerMoveSpeed playerStats) (playerBulletSpeed playerStats) playerRange
            in ([enemy], wave { enemiesLeft = enemiesLeft wave - 1, timeSinceSpawn = 0 })
       else ([], wave { timeSinceSpawn = newTime })