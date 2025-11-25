-- Main.hs
-- Entry point for the space game

module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import GameState
import Input
import Render
import Assets
import Update (updateWorld) -- actualizar balas, enemigos, oleadas

-- Window configuration
windowWidth, windowHeight :: Int
windowWidth = 640  -- Actualizado al nuevo ancho de terreno
windowHeight = 360

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
         updateWorld    -- ⬅️ ESTA es la función correcta
