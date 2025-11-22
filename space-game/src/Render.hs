-- Render.hs
-- Render the game state to the screen
module Render
  ( render
  ) where

import Graphics.Gloss
import GameState
import Assets
import Enemy

-------------------------------------------------------------
-- RENDER GENERAL
-------------------------------------------------------------

render :: Assets -> GameState -> Picture
render assets gs =
    pictures [ scaledBackground
             , drawEnemies gs
             , drawPlayer assets gs
             ]
  where
    -- Escala según el ancho de la ventana
    (winW, _winH) = windowSize gs
    baseSize = 800.0
    scaleF = fromIntegral winW / baseSize

    background = aBackground assets
    scaledBackground = scale scaleF scaleF background


-------------------------------------------------------------
-- RENDER DEL JUGADOR
-------------------------------------------------------------

drawPlayer :: Assets -> GameState -> Picture
drawPlayer assets gs =
    translate px py $
        scale scaleF scaleF $
            playerSprite
  where
    (px, py) = playerPos gs

    -- frames del jugador
    frames = aPlayerFrames assets
    numFrames = max 1 (length frames)

    frameIndex = floor (animTime gs * 8.0) `mod` numFrames
    playerSprite = frames !! frameIndex

    -- escala de pantalla
    (winW, _) = windowSize gs
    scaleF = fromIntegral winW / 800.0


-------------------------------------------------------------
-- RENDER DE ENEMIGOS
-------------------------------------------------------------

drawEnemies :: GameState -> Picture
drawEnemies gs = pictures (map drawEnemy (enemies gs))
  where
    (winW, _) = windowSize gs
    scaleF = fromIntegral winW / 800.0

    drawEnemy enemy =
        let (x, y) = enemyPos enemy
        in translate x y $ scale scaleF scaleF $
             color red (circleSolid 12)
