-- src/Main.hs
-- Entry point for the space game

module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import GameState
import Input
import Render
import Assets
import Update (updateWorld) 

-- Window configuration
windowWidth, windowHeight :: Int
windowWidth = 1600
windowHeight = 900

window :: Display
window = InWindow "Space Game - Haskell"
                 (windowWidth, windowHeight)
                 (100, 100)

backgroundColor :: Color
backgroundColor = black

fps :: Int
fps = 60

-- FUNCIÓN CLAVE: Detiene la actualización del mundo si estamos en el Menú
safeUpdateWorld :: Float -> GameState -> GameState
safeUpdateWorld dt gs
    | currentScreen gs == Playing = updateWorld dt gs
    | otherwise                   = gs -- ¡Pausa lógica en el menú!

main :: IO ()
main = do
    putStrLn "Loading assets..."
    assets <- loadAssets
    putStrLn "Assets loaded successfully!"

    let initialGS = initialState { windowSize = (windowWidth, windowHeight) }

    play window
         backgroundColor
         fps
         initialGS
         (render assets)     -- Dibuja (Render.hs decide si Menu o Juego)
         handleEvent         -- Inputs (Input.hs maneja el clic en Menu)
         safeUpdateWorld     -- Actualiza (Solo si Playing)