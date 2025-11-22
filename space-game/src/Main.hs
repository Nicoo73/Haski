-- Main.hs
-- Entry point for the space game
module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import GameState
import Input
import Render
import Assets

-- Window configuration
windowWidth, windowHeight :: Int
windowWidth = 480
windowHeight = 360

window :: Display
window = InWindow "Space Game - Haskell" (windowWidth, windowHeight) (100, 100)

backgroundColor :: Color
backgroundColor = black

fps :: Int
fps = 60

main :: IO ()
main = do
  -- Load assets
  putStrLn "Loading assets..."
  assets <- loadAssets
  putStrLn "Assets loaded successfully!"
  
  -- Initialize game state
  let initialGS = initialState { windowSize = (windowWidth, windowHeight) }
  
  -- Start game loop
  play window backgroundColor fps initialGS (render assets) handleEvent updateGameState
