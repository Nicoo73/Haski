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
windowWidth = 480
windowHeight = 360

window :: Display
window = InWindow "Space Game - Haskell"
                 (windowWidth, windowHeight)
                 (100, 100)

backgroundColor :: Color
backgroundColor = black

fps :: Int
fps = 60

-- Nueva función de actualización: solo actualiza el mundo si el estado es Playing
safeUpdateWorld :: Float -> GameState -> GameState
safeUpdateWorld dt gs
    | currentScreen gs == Playing = updateWorld dt gs
    | otherwise                   = gs -- No actualiza nada en el menú

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
         --updateWorld    -- ⬅️ ESTA es la función correcta
         safeUpdateWorld     -- función de actualización condicional