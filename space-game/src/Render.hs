-- Render.hs
-- Render the game state to the screen
module Render
  ( render
  ) where

import Graphics.Gloss
import GameState
import Assets
import Enemy
import Item 

-------------------------------------------------------------
-- RENDER GENERAL
-------------------------------------------------------------

render :: Assets -> GameState -> Picture
render assets gs =
    pictures [ scaledBackground
             , drawItems gs    
             , drawEnemies assets gs
             , drawBullets gs
             , drawEnemyBullets gs
             , scaledPlayer
             , drawHUD gs      -- FUNCIÓN CORREGIDA
             ]
  where
    -- Escala según el ancho de la ventana
    (winW, winH) = windowSize gs

    terrainW = 640.0 
    terrainH = 360.0
    -- Use minimum scale to ensure terrain fits in both dimensions
    scaleW = fromIntegral winW / terrainW
    scaleH = fromIntegral winH / terrainH
    scaleF = min scaleW scaleH -- Factor de escala para todo el mapa

    background = aBackground assets
    scaledBackground = scale scaleF scaleF background

    -- Get rotation angle based on player direction (8 directions)
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

    -- frames del jugador
    frames = aPlayerFrames assets
    numFrames = max 1 (length frames)

    frameIndex = floor (animTime gs * 8.0) `mod` numFrames
    playerSprite = frames !! frameIndex

    -- Transformación: Escala -> Rotación -> Traslación
    scaledPlayer = translate (px * scaleF) (py * scaleF) $ scale scaleF scaleF $ rotate rotAngle playerSprite

    
-------------------------------------------------------------
-- RENDER DE ITEMS
-------------------------------------------------------------

drawItems :: GameState -> Picture
drawItems gs = pictures (map drawItem (items gs))
  where
    (winW, winH) = windowSize gs
    terrainW = 640.0
    terrainH = 360.0
    scaleW = fromIntegral winW / terrainW
    scaleH = fromIntegral winH / terrainH
    scaleF = min scaleW scaleH 

    -- Usamos el radio que define el tamaño del ítem en el mundo (10.0)
    drawItem item =
      let (x,y) = itemPos item
          radius = itemRadius item
          (col, shape) = case itemType item of
              HealSmall   -> (green, circleSolid radius)
              SpeedBoost  -> (blue, rectangleSolid radius radius)
              DamageBoost -> (violet, rotate 45 $ rectangleSolid radius radius)
      -- APLICAMOS EL MISMO FACTOR DE ESCALA AL DIBUJAR
      in translate (x * scaleF) (y * scaleF) $ scale scaleF scaleF $ color col shape

-------------------------------------------------------------
-- RENDER DE PROYECTILES
-------------------------------------------------------------

drawBullets :: GameState -> Picture
drawBullets gs = pictures (map drawBullet (bullets gs))
  where
    (winW, winH) = windowSize gs
    terrainW = 640.0
    terrainH = 360.0
    scaleW = fromIntegral winW / terrainW
    scaleH = fromIntegral winH / terrainH
    scaleF = min scaleW scaleH

    drawBullet bullet =
        let (x, y) = bulletPos bullet
        in translate (x * scaleF) (y * scaleF) $ scale scaleF scaleF $
             color yellow (circleSolid 3) 

-------------------------------------------------------------
-- RENDER DE PROYECTILES ENEMIGOS
-------------------------------------------------------------

drawEnemyBullets :: GameState -> Picture
drawEnemyBullets gs = pictures (map drawEnemyBullet (enemyBullets gs))
  where
    (winW, winH) = windowSize gs
    terrainW = 640.0
    terrainH = 360.0
    scaleW = fromIntegral winW / terrainW
    scaleH = fromIntegral winH / terrainH
    scaleF = min scaleW scaleH

    drawEnemyBullet eBullet =
        let (x, y) = eBulletPos eBullet
        in translate (x * scaleF) (y * scaleF) $ scale scaleF scaleF $
             color red (circleSolid 3) 

-------------------------------------------------------------
-- RENDER DE ENEMIGOS
-------------------------------------------------------------

drawEnemies :: Assets -> GameState -> Picture
drawEnemies assets gs = pictures (map drawEnemy (enemies gs))
  where
    (winW, winH) = windowSize gs
    terrainW = 640.0
    terrainH = 360.0
    scaleW = fromIntegral winW / terrainW
    scaleH = fromIntegral winH / terrainH
    scaleF = min scaleW scaleH

    drawEnemy enemy =
        let (x, y) = enemyPos enemy
            enemySprite = case enemyType enemy of
              Caza -> aCazaSprite assets
        in translate (x * scaleF) (y * scaleF) $ scale scaleF scaleF $ enemySprite

-------------------------------------------------------------
-- HUD (REPOSICIONADO Y ESPACIADO)
-------------------------------------------------------------

drawHUD :: GameState -> Picture
drawHUD gs@GameState{ currentStats = cs, currentHealth = ch, waveCount = wc } =
  let
    -- Cálculo de datos (sin cambios)
    totalDmg = playerDamage cs + playerDamageBonus cs
    totalSpd = playerMoveSpeed cs + playerSpeedBonus cs
    
    -- Texto del HUD (sin cambios)
    healthText = "HP: " ++ show ch
    damageText = "Dmg: " ++ show totalDmg ++ " (+" ++ show (playerDamageBonus cs) ++ ")"
    speedText  = "Spd: " ++ show (round totalSpd) ++ " (+" ++ show (round (playerSpeedBonus cs)) ++ ")"
    waveText   = "Wave: " ++ show wc
    
    -- Ajuste de diseño:
    hudScale = 0.15 
    lineHeight = 50.0 -- Separación vertical AUMENTADA para evitar solapamiento
    
    -- El texto de Gloss se centra. Para alinearlo a la izquierda,
    -- movemos el punto de anclaje (0,0) hacia la derecha una cantidad 'offset'
    -- (el offset es aproximado al ancho de una línea).
    textHorizontalOffset = 180.0 / hudScale -- Estimación del ancho del texto (180.0)

    -- La posición base es la esquina superior izquierda del área de juego
    -- (-terrainW/2 + margin, terrainH/2 - margin).
    -- Usamos -300 para X y 160 para Y. 
    -- Luego, movemos las líneas individualmente para que la traducción base funcione.

    hudPos = (-300, 160) 
    
    -- Organizar el texto: 
    -- Trasladamos la línea hacia la derecha (Offset) para que el centroide del texto 
    -- se mueva a la derecha y el texto se perciba alineado a la izquierda.
    line1 = translate (textHorizontalOffset / 2) 0 $ text healthText
    line2 = translate (textHorizontalOffset / 2) (-lineHeight) $ text damageText
    line3 = translate (textHorizontalOffset / 2) (-lineHeight * 2) $ text speedText
    line4 = translate (textHorizontalOffset / 2) (-lineHeight * 3) $ text waveText
    
  in translate (fst hudPos) (snd hudPos) $ scale hudScale hudScale $ color white $ 
     pictures [ line1, line2, line3, line4 ]