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
             , drawEnemies assets gs
             , drawBullets gs
             , drawEnemyBullets gs
             , scaledPlayer
             ]
  where
    -- Escala según el ancho de la ventana
    (winW, winH) = windowSize gs

    terrainW = 640.0  -- Actualizado
    terrainH = 360.0
    -- Use minimum scale to ensure terrain fits in both dimensions
    scaleW = fromIntegral winW / terrainW
    scaleH = fromIntegral winH / terrainH
    scaleF = min scaleW scaleH

    background = aBackground assets
    scaledBackground = scale scaleF scaleF background

    -- Get rotation angle based on player direction (8 directions)
    rotAngle = case playerDir gs of
      DUp        -> 0
      DDown      -> 180
      DLeft      -> 270
      DRight     -> 90
      DUpLeft    -> 315  -- 45° hacia arriba-izquierda
      DUpRight   -> 45   -- 45° hacia arriba-derecha
      DDownLeft  -> 225  -- 45° hacia abajo-izquierda
      DDownRight -> 135  -- 45° hacia abajo-derecha



    (px, py) = playerPos gs

    -- frames del jugador
    frames = aPlayerFrames assets
    numFrames = max 1 (length frames)

    frameIndex = floor (animTime gs * 8.0) `mod` numFrames
    playerSprite = frames !! frameIndex

    scaledPlayer = translate (px * scaleF) (py * scaleF) $ scale scaleF scaleF $ rotate rotAngle playerSprite

    
-------------------------------------------------------------
-- RENDER DE PROYECTILES (NUEVA SECCIÓN)
-------------------------------------------------------------

drawBullets :: GameState -> Picture
drawBullets gs = pictures (map drawBullet (bullets gs))
  where
    (winW, winH) = windowSize gs
    terrainW = 640.0  -- Actualizado
    terrainH = 360.0
    scaleW = fromIntegral winW / terrainW
    scaleH = fromIntegral winH / terrainH
    scaleF = min scaleW scaleH

    drawBullet bullet =
        let (x, y) = bulletPos bullet
        in translate (x * scaleF) (y * scaleF) $ scale scaleF scaleF $
             color yellow (circleSolid 3) -- Dibujar un punto amarillo (3 píxeles)

-------------------------------------------------------------
-- RENDER DE PROYECTILES ENEMIGOS
-------------------------------------------------------------

drawEnemyBullets :: GameState -> Picture
drawEnemyBullets gs = pictures (map drawEnemyBullet (enemyBullets gs))
  where
    (winW, winH) = windowSize gs
    terrainW = 640.0  -- Actualizado
    terrainH = 360.0
    scaleW = fromIntegral winW / terrainW
    scaleH = fromIntegral winH / terrainH
    scaleF = min scaleW scaleH

    drawEnemyBullet eBullet =
        let (x, y) = eBulletPos eBullet
        in translate (x * scaleF) (y * scaleF) $ scale scaleF scaleF $
             color red (circleSolid 3) -- Dibujar un punto rojo (3 píxeles)

-------------------------------------------------------------
-- RENDER DE ENEMIGOS
-------------------------------------------------------------

drawEnemies :: Assets -> GameState -> Picture
drawEnemies assets gs = pictures (map drawEnemy (enemies gs))
  where
    (winW, winH) = windowSize gs
    terrainW = 640.0  -- Actualizado
    terrainH = 360.0
    scaleW = fromIntegral winW / terrainW
    scaleH = fromIntegral winH / terrainH
    scaleF = min scaleW scaleH

    drawEnemy enemy =
        let (x, y) = enemyPos enemy
            enemySprite = case enemyType enemy of
              Caza -> aCazaSprite assets
            -- Sprite estático sin rotación
        in translate (x * scaleF) (y * scaleF) $ scale scaleF scaleF $ enemySprite
