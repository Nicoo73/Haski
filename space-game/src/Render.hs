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
             , drawEnemies gs
             , drawBullets gs
             , scaledPlayer
             , drawHUD gs      -- Pantalla de info (HUD)
             ]
  where
    (winW, winH) = windowSize gs

    terrainW = 480.0
    terrainH = 360.0
    scaleW = fromIntegral winW / terrainW
    scaleH = fromIntegral winH / terrainH
    scaleF = min scaleW scaleH

    background = aBackground assets
    scaledBackground = scale scaleF scaleF background

    rotAngle = case playerDir gs of
      DUp    -> 0
      DDown  -> 180
      DLeft  -> 270
      DRight -> 90

    (px, py) = playerPos gs

    frames = aPlayerFrames assets
    numFrames = max 1 (length frames)
    frameIndex = floor (animTime gs * 8.0) `mod` numFrames
    playerSprite = frames !! frameIndex

    scaledPlayer = translate (px * scaleF) (py * scaleF) $ scale scaleF scaleF $ rotate rotAngle playerSprite

-------------------------------------------------------------
-- RENDER DE ITEMS
-------------------------------------------------------------

drawItems :: GameState -> Picture
drawItems gs = pictures (map drawItem (items gs))
  where
    (winW, _) = windowSize gs
    scaleF = fromIntegral winW / 800.0 

    drawItem item =
      let (x,y) = itemPos item
          (col, shape) = case itemType item of
              HealSmall   -> (green, circleSolid 8)
              SpeedBoost  -> (azure, rectangleSolid 10 10)
              DamageBoost -> (violet, rotate 45 $ rectangleSolid 9 9) -- Violeta para daño
      in translate x y $ scale scaleF scaleF $ color col shape

-------------------------------------------------------------
-- RENDER DE PROYECTILES
-------------------------------------------------------------

drawBullets :: GameState -> Picture
drawBullets gs = pictures (map drawBullet (bullets gs))
  where
    (winW, _) = windowSize gs
    scaleF = fromIntegral winW / 800.0 

    drawBullet bullet =
        let (x, y) = bulletPos bullet
        in translate x y $ scale scaleF scaleF $
             color yellow (circleSolid 3)

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

-------------------------------------------------------------
-- HUD SIMPLE (Vida y DAÑO)
-------------------------------------------------------------
drawHUD :: GameState -> Picture
drawHUD gs = translate (-200) 150 $ scale 0.15 0.15 $ color white $ 
             text ("HP: " ++ show (playerHP gs) ++ " | Dmg: " ++ show (playerDamage gs))