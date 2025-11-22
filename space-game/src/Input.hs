-- Input.hs
-- Handle keyboard input, player movement, and enemy/wave updates

module Input
  ( handleEvent
  , updateGameState
  ) where

import Graphics.Gloss.Interface.Pure.Game
import GameState
import Enemy
import Wave

-------------------------------------------------------------
-- EVENTOS DE TECLAS
-------------------------------------------------------------

handleEvent :: Event -> GameState -> GameState
handleEvent (EventKey (Char c) keyState _ _) gs = case (c, keyState) of
  ('w', Down) -> addKey DUp gs
  ('a', Down) -> addKey DLeft gs
  ('s', Down) -> addKey DDown gs
  ('d', Down) -> addKey DRight gs
  ('w', Up)   -> removeKey DUp gs
  ('a', Up)   -> removeKey DLeft gs
  ('s', Up)   -> removeKey DDown gs
  ('d', Up)   -> removeKey DRight gs
  _           -> gs

handleEvent (EventResize newSize) gs =
  gs { windowSize = newSize }

handleEvent _ gs = gs

-------------------------------------------------------------
-- UPDATE GENERAL DEL JUEGO
-------------------------------------------------------------

updateGameState :: Float -> GameState -> GameState
updateGameState dt =
    updateEnemies dt      -- mover enemigos
  . updateWave dt         -- spawnear enemigos
  . updatePlayer dt       -- mover jugador

-------------------------------------------------------------
-- SISTEMA DE MOVIMIENTO DEL JUGADOR
-------------------------------------------------------------

updatePlayer :: Float -> GameState -> GameState
updatePlayer dt gs = gs
  { playerPos = newPos
  , animTime = animTime gs + dt
  }
  where
    (dx, dy) = foldr addVector (0, 0) (map dirToVector (keysDown gs))

    magnitude = sqrt (dx*dx + dy*dy)
    (ndx, ndy) = if magnitude > 0 then (dx/magnitude, dy/magnitude) else (0,0)

    moveX = ndx * playerSpeed * dt
    moveY = ndy * playerSpeed * dt

    (px, py) = playerPos gs
    (winW, winH) = windowSize gs

    half = playerSize / 2
    minX = -fromIntegral winW / 2 + half
    maxX =  fromIntegral winW / 2 - half
    minY = -fromIntegral winH / 2 + half
    maxY =  fromIntegral winH / 2 - half

    newX = clamp (px + moveX) minX maxX
    newY = clamp (py + moveY) minY maxY
    newPos = (newX, newY)

    addVector (a,b) (c,d) = (a+c, b+d)

-------------------------------------------------------------
-- SISTEMA DE ENEMIGOS (con Enemy.hs moderno)
-------------------------------------------------------------

updateEnemies :: Float -> GameState -> GameState
updateEnemies dt gs@GameState{ enemies = es, playerPos = p } =
  gs { enemies = map (updateEnemy dt p) es }

-------------------------------------------------------------
-- SISTEMA DE OLEADAS (con Wave.hs moderno)
-------------------------------------------------------------

updateWave :: Float -> GameState -> GameState
updateWave dt gs@GameState{ wave = w, enemies = es } =
  let (spawned, newWave) = spawnWaveIfNeeded dt w
  in gs { enemies = es ++ spawned, wave = newWave }

-------------------------------------------------------------
-- UTILIDADES
-------------------------------------------------------------

dirToVector :: Direction -> (Float, Float)
dirToVector DUp    = (0, 1)
dirToVector DDown  = (0,-1)
dirToVector DLeft  = (-1,0)
dirToVector DRight = (1, 0)

clamp :: Float -> Float -> Float -> Float
clamp x a b = max a (min b x)
