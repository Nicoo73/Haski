-- Input.hs
-- Handle keyboard input and player movement
module Input
  ( handleEvent
  , updateGameState
  ) where

import Graphics.Gloss.Interface.Pure.Game
import GameState

-- Handle keyboard events
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
handleEvent (EventResize newSize) gs = gs { windowSize = newSize }
handleEvent _ gs = gs

-- Add a direction to the pressed keys list
addKey :: Direction -> GameState -> GameState
addKey dir gs = gs { keysDown = dir : filter (/= dir) (keysDown gs) }

-- Remove a direction from the pressed keys list
removeKey :: Direction -> GameState -> GameState
removeKey dir gs = gs { keysDown = filter (/= dir) (keysDown gs) }

-- Update game state each frame
updateGameState :: Float -> GameState -> GameState
updateGameState dt gs = gs
  { playerPos = newPos
  , animTime  = animTime gs + dt
  }
  where
    -- Calculate movement vector from pressed keys
    (dx, dy) = foldr addVector (0, 0) (map dirToVector (keysDown gs))
    
    -- Normalize diagonal movement
    magnitude = sqrt (dx * dx + dy * dy)
    (ndx, ndy) = if magnitude > 0
                 then (dx / magnitude, dy / magnitude)
                 else (0, 0)
    
    -- Apply movement with speed
    moveX = ndx * playerSpeed * dt
    moveY = ndy * playerSpeed * dt
    
    (px, py) = playerPos gs
    (winW, winH) = windowSize gs
    
    -- Calculate boundaries (keep player on screen)
    halfSize = playerSize / 2
    minX = -fromIntegral winW / 2 + halfSize
    maxX = fromIntegral winW / 2 - halfSize
    minY = -fromIntegral winH / 2 + halfSize
    maxY = fromIntegral winH / 2 - halfSize
    
    -- Clamp position to boundaries
    newX = clamp (px + moveX) minX maxX
    newY = clamp (py + moveY) minY maxY
    newPos = (newX, newY)
    
    addVector (a, b) (c, d) = (a + c, b + d)

-- Convert direction to unit vector
dirToVector :: Direction -> (Float, Float)
dirToVector DUp    = (0, 1)
dirToVector DDown  = (0, -1)
dirToVector DLeft  = (-1, 0)
dirToVector DRight = (1, 0)

-- Clamp a value between min and max
clamp :: Float -> Float -> Float -> Float
clamp x minVal maxVal = max minVal (min maxVal x)
