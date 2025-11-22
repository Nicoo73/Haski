-- Render.hs
-- Render the game state to the screen
module Render
  ( render
  ) where

import Graphics.Gloss
import GameState
import Assets

-- Render the complete game scene
render :: Assets -> GameState -> Picture
render assets gs = pictures [scaledBackground, scaledPlayer]
  where
    -- Calculate scale factor based on window size
    (winW, winH) = windowSize gs
    baseSize = 800.0  -- Base window width for scale factor 1.0
    scaleF = fromIntegral winW / baseSize
    
    -- Render background (scaled)
    background = aBackground assets
    scaledBackground = scale scaleF scaleF background
    
    -- Render player with animation (scaled)
    (px, py) = playerPos gs
    frames = aPlayerFrames assets
    numFrames = max 1 (length frames)
    
    -- Cycle through frames at 8 FPS
    frameIndex = floor (animTime gs * 8.0) `mod` numFrames
    playerSprite = frames !! frameIndex
    
    scaledPlayer = translate (px * scaleF) (py * scaleF) $ scale scaleF scaleF playerSprite
