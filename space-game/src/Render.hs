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
    terrainW = 480.0
    terrainH = 360.0
    -- Use minimum scale to ensure terrain fits in both dimensions
    scaleW = fromIntegral winW / terrainW
    scaleH = fromIntegral winH / terrainH
    scaleF = min scaleW scaleH
    
    -- Render background (scaled)
    background = aBackground assets
    scaledBackground = scale scaleF scaleF background
    
    -- Get rotation angle based on player direction
    rotAngle = case playerDir gs of
      DUp    -> 0
      DDown  -> 180
      DLeft  -> 270
      DRight -> 90
    
    -- Render player with animation (scaled and rotated)
    (px, py) = playerPos gs
    frames = aPlayerFrames assets
    numFrames = max 1 (length frames)
    
    -- Cycle through frames at 8 FPS
    frameIndex = floor (animTime gs * 8.0) `mod` numFrames
    playerSprite = frames !! frameIndex
    
    scaledPlayer = translate (px * scaleF) (py * scaleF) $ scale scaleF scaleF $ rotate rotAngle playerSprite
