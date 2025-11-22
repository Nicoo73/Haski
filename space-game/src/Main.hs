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
windowWidth = 800
windowHeight = 600

window :: Display
window = InWindow "Space Game - Haskell"
                 (windowWidth, windowHeight)
                 (100, 100)

backgroundColor :: Color
backgroundColor = black

fps :: Int
fps = 60

main :: IO ()
main = do
    putStrLn "Loading assets..."
    assets <- loadAssets
    putStrLn "Assets loaded successfully!"

    -- Initial game state
    let initialGS = initialState { windowSize = (windowWidth, windowHeight) }

    -- Game loop
    play window
         backgroundColor
         fps
         initialGS
         (render assets)     -- función para dibujar
         handleEvent         -- eventos de teclado
         updateGameState     -- ⬅️ ESTA es la función correcta
