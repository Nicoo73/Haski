module Update
  ( updateWorld
  ) where

import GameState
import Enemy
import Wave
import Input (updateGameState)

updateWorld :: Float -> GameState -> GameState
updateWorld dt gs =
  let gs1 = updateGameState dt gs
      (spawned, newWave) = spawnWaveIfNeeded dt (wave gs1)
      movedEnemies = map (updateEnemy dt (playerPos gs1))
                         (spawned ++ enemies gs1)
  in gs1 { enemies = movedEnemies, wave = newWave }
