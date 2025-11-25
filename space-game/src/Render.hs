-- src/Render.hs

module Render
  ( render
  ) where

import Graphics.Gloss
import GameState
import Assets
import Enemy

-------------------------------------------------------------
-- CONSTANTES DEL MENÚ
-------------------------------------------------------------

-- Posición del botón (centro del mapa)
buttonX, buttonY :: Float
buttonX = 0.0
buttonY = -50.0

-- Tamaño del botón
buttonW, buttonH :: Float
buttonW = 300.0
buttonH = 60.0

-- Dimensiones originales de la imagen de fondo del menú
menuBgW, menuBgH :: Float
menuBgW = 1920.0 
menuBgH = 1080.0 

-------------------------------------------------------------
-- RENDER GENERAL
-------------------------------------------------------------

render :: Assets -> GameState -> Picture
render assets gs = case currentScreen gs of
    Menu    -> drawMenuScreen assets gs
    Playing -> drawGameScreen assets gs

drawGameScreen :: Assets -> GameState -> Picture
drawGameScreen assets gs =
    pictures [ scaledBackground
             , drawEnemies gs
             , drawBullets gs
             , scaledPlayer
             ]
  where
    (winW, winH) = windowSize gs

    -- Definimos el tamaño lógico del terreno
    terrainW = 480.0
    terrainH = 360.0
    
    -- CAMBIO IMPORTANTE: Escalado independiente (Estirar)
    -- Calculamos la escala en X e Y por separado para llenar toda la ventana
    scaleX = fromIntegral winW / terrainW
    scaleY = fromIntegral winH / terrainH

    background = aBackground assets
    scaledBackground = scale scaleX scaleY background

    -- Rotación del jugador
    rotAngle = case playerDir gs of
      DUp        -> 0
      DDown      -> 180
      DLeft      -> 270
      DRight     -> 90
      DUpLeft    -> 315 
      DUpRight   -> 45  
      DDownLeft  -> 225 
      DDownRight -> 135 

    (px, py) = playerPos gs

    frames = aPlayerFrames assets
    numFrames = max 1 (length frames)
    frameIndex = floor (animTime gs * 8.0) `mod` numFrames
    playerSprite = frames !! frameIndex

    -- Aplicamos el escalado independiente a la posición y al sprite
    scaledPlayer = translate (px * scaleX) (py * scaleY) $ scale scaleX scaleY $ rotate rotAngle playerSprite

    
-------------------------------------------------------------
-- RENDER DE MENÚ
-------------------------------------------------------------

drawMenuScreen :: Assets -> GameState -> Picture
drawMenuScreen assets gs =
    pictures [ scaledMenuBackground
             , drawMenuButton
             ]
  where
    (winWInt, winHInt) = windowSize gs
    winW = fromIntegral winWInt
    winH = fromIntegral winHInt
    
    -- Escalado Estirado para el menú también
    menuScaleW = winW / menuBgW
    menuScaleH = winH / menuBgH
    
    scaledMenuBackground = scale menuScaleW menuScaleH (aMenuBackground assets)

    drawMenuButton =
        let 
            buttonOutline = color white (rectangleWire buttonW buttonH)
            buttonFill = color (makeColorI 0 0 128 255) (rectangleSolid (buttonW - 2) (buttonH - 2))
            buttonText = translate (-120) (-10) $ scale 0.2 0.2 $ color white $ text "Comenzar a jugar"
        in translate buttonX buttonY $ pictures [buttonFill, buttonOutline, buttonText]

-------------------------------------------------------------
-- RENDER DE PROYECTILES
-------------------------------------------------------------

drawBullets :: GameState -> Picture
drawBullets gs = pictures (map drawBullet (bullets gs))
  where
    (winW, winH) = windowSize gs
    terrainW = 480.0
    terrainH = 360.0
    
    -- Usamos el mismo factor de estiramiento que en el juego
    scaleX = fromIntegral winW / terrainW
    scaleY = fromIntegral winH / terrainH

    drawBullet bullet =
        let (x, y) = bulletPos bullet
        in translate (x * scaleX) (y * scaleY) $ scale scaleX scaleY $
             color yellow (circleSolid 3)

-------------------------------------------------------------
-- RENDER DE ENEMIGOS
-------------------------------------------------------------

drawEnemies :: GameState -> Picture
drawEnemies gs = pictures (map drawEnemy (enemies gs))
  where
    (winW, winH) = windowSize gs
    terrainW = 480.0
    terrainH = 360.0
    
    -- Usamos el mismo factor de estiramiento que en el juego
    scaleX = fromIntegral winW / terrainW
    scaleY = fromIntegral winH / terrainH

    drawEnemy enemy =
        let (x, y) = enemyPos enemy
        in translate (x * scaleX) (y * scaleY) $ scale scaleX scaleY $
             color red (circleSolid 12)