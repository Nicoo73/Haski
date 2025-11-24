module Wave
  ( Wave(..)
  , initialWave
  , spawnWaveIfNeeded
  , nextWave        -- Exportado para que Update.hs pueda avanzar la oleada
  , enemiesForWave  -- Exportado para referencia futura
  ) where

import Enemy

-------------------------------------------------------------
-- TIPOS Y CONSTANTES
-------------------------------------------------------------

data Wave = Wave
  { enemiesLeft    :: Int      -- Enemigos restantes por generar
  , timeSinceSpawn :: Float    -- Tiempo desde que se generó el último enemigo
  } deriving (Show)

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

initialWave :: Wave
initialWave = nextWave 1 -- La oleada inicial es la Oleada 1 (20 enemigos)

-------------------------------------------------------------
-- LÓGICA DE SPAWN
-------------------------------------------------------------

-- Genera un enemigo si el tiempo lo permite y quedan enemigos por spawnear
spawnWaveIfNeeded :: Float -> Wave -> ([Enemy], Wave)
spawnWaveIfNeeded dt wave =
  let newTime = timeSinceSpawn wave + dt
  in if enemiesLeft wave > 0 && newTime >= 1 -- Genera uno cada 1 segundo
       then let enemy = Enemy { enemyPos = (0,200), enemySpeed = 60 }
            in ([enemy], wave { enemiesLeft = enemiesLeft wave - 1, timeSinceSpawn = 0 })
       else ([], wave { timeSinceSpawn = newTime })