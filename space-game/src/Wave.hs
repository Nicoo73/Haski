module Wave
  ( Wave(..)
  , initialWave
  , spawnWaveIfNeeded
  ) where

import Enemy

data Wave = Wave
  { waveNumber     :: Int
  , enemiesLeft    :: Int
  , timeSinceSpawn :: Float
  } deriving (Show)

initialWave :: Wave
initialWave = Wave
  { waveNumber     = 1
  , enemiesLeft    = 5
  , timeSinceSpawn = 0
  }

spawnWaveIfNeeded :: Float -> Wave -> ([Enemy], Wave)
spawnWaveIfNeeded dt wave =
  let newTime = timeSinceSpawn wave + dt
  in if enemiesLeft wave > 0 && newTime >= 1
       then let enemy = Enemy { enemyPos = (0,200), enemySpeed = 60 }
            in ([enemy], wave { enemiesLeft = enemiesLeft wave - 1, timeSinceSpawn = 0 })
       else ([], wave { timeSinceSpawn = newTime })
