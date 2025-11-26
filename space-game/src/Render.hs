module Render
  ( render
  ) where

import Graphics.Gloss
import GameState
import Assets
import Enemy
import Item 

-------------------------------------------------------------
-- CONSTANTES DEL MENÚ
-------------------------------------------------------------

buttonX, buttonY :: Float
buttonX = 0.0
buttonY = -50.0

buttonW, buttonH :: Float
buttonW = 300.0
buttonH = 60.0

menuBgW, menuBgH :: Float
menuBgW = 1024.0 
menuBgH = 572.0 

-------------------------------------------------------------
-- RENDER GENERAL
-------------------------------------------------------------

render :: Assets -> GameState -> Picture
render assets gs = case currentScreen gs of
    Menu    -> drawMenuScreen assets gs
    Playing -> drawGameScreen assets gs
    GameOver -> drawGameScreen assets gs

drawGameScreen :: Assets -> GameState -> Picture
drawGameScreen assets gs =
    pictures [ scaledBackground
             , drawItems gs    
             , drawEnemies assets gs
             , drawBullets gs
             , drawEnemyBullets gs
             , scaledPlayer
             , drawHUD gs -- Usa drawHUD o drawHealthBar, aquí unificamos a drawHealthBar/HUD
             ]
  where
    (winW, winH) = windowSize gs

    terrainW = 640.0 
    terrainH = 360.0
    
    scaleX = fromIntegral winW / terrainW
    scaleY = fromIntegral winH / terrainH

    background = aBackground assets
    scaledBackground = scale scaleX scaleY background

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
    
    menuScaleW = winW / menuBgW
    menuScaleH = winH / menuBgH
    menuScaleF = max menuScaleW menuScaleH 

    scaledMenuBackground = scale menuScaleF menuScaleF (aMenuBackground assets)

    drawMenuButton =
        let 
            buttonOutline = color white (rectangleWire buttonW buttonH)
            buttonFill = color (makeColorI 0 0 128 255) (rectangleSolid (buttonW - 2) (buttonH - 2))
            buttonText = translate (-120) (-10) $ scale 0.2 0.2 $ color white $ text "Comenzar a jugar"
        in translate buttonX buttonY $ pictures [buttonFill, buttonOutline, buttonText]

-------------------------------------------------------------
-- RENDER DE HUD / BARRA DE VIDA (CORREGIDO)
-------------------------------------------------------------

-- Puedes renombrar esto a drawHealthBar si prefieres solo la barra, 
-- o mantener drawHUD si quieres mostrar más info. Aquí pongo la barra simple corregida.
drawHUD :: GameState -> Picture 
drawHUD gs = 
    translate barX barY $ 
    pictures [ bgBar, fillBar, borderBar ]
  where
    (winWInt, winHInt) = windowSize gs
    
    maxHP = 100.0
    -- ¡CORRECCIÓN AQUÍ! Usamos currentHealth gs
    currentHP = fromIntegral (currentHealth gs)
    barW = 100.0
    barH = 10.0
    
    margin = 20.0
    barX = -fromIntegral winWInt / 2 + margin + (barW / 2)
    barY = fromIntegral winHInt / 2 - margin - (barH / 2)

    fillPct = max 0 (currentHP / maxHP)
    fillW = barW * fillPct

    bgBar = color red $ rectangleSolid barW barH
    fillBar = translate (-(barW - fillW)/2) 0 $
              color green $ rectangleSolid fillW barH
    borderBar = color white $ rectangleWire barW barH

-------------------------------------------------------------
-- RENDER DE PROYECTILES
-------------------------------------------------------------

drawBullets :: GameState -> Picture
drawBullets gs = pictures (map drawBullet (bullets gs))
  where
    (winW, winH) = windowSize gs
    terrainW = 640.0
    terrainH = 360.0
    scaleX = fromIntegral winW / terrainW
    scaleY = fromIntegral winH / terrainH

    drawBullet bullet =
        let (x, y) = bulletPos bullet
        in translate (x * scaleX) (y * scaleY) $ scale scaleX scaleY $
             color yellow (circleSolid 3)

drawEnemyBullets :: GameState -> Picture
drawEnemyBullets gs = pictures (map drawEnemyBullet (enemyBullets gs))
  where
    (winW, winH) = windowSize gs
    terrainW = 640.0
    terrainH = 360.0
    scaleX = fromIntegral winW / terrainW
    scaleY = fromIntegral winH / terrainH

    drawEnemyBullet eBullet =
        let (x, y) = eBulletPos eBullet
        in translate (x * scaleX) (y * scaleY) $ scale scaleX scaleY $
             color red (circleSolid 3)

-------------------------------------------------------------
-- RENDER DE ENEMIGOS Y ITEMS
-------------------------------------------------------------

drawEnemies :: Assets -> GameState -> Picture
drawEnemies assets gs = pictures (map drawEnemy (enemies gs))
  where
    (winW, winH) = windowSize gs
    terrainW = 640.0
    terrainH = 360.0
    scaleX = fromIntegral winW / terrainW
    scaleY = fromIntegral winH / terrainH

    drawEnemy enemy =
        let (x, y) = enemyPos enemy
            enemySprite = aCazaSprite assets 
        in translate (x * scaleX) (y * scaleY) $ scale scaleX scaleY $ enemySprite

drawItems :: GameState -> Picture
drawItems gs = pictures (map drawItem (items gs))
  where
    (winW, winH) = windowSize gs
    scaleX = fromIntegral winW / 640.0
    scaleY = fromIntegral winH / 360.0

    drawItem item =
      let (x,y) = itemPos item
          (col, shape) = (green, circleSolid 10.0)
      in translate (x * scaleX) (y * scaleY) $ scale scaleX scaleY $ color col shape