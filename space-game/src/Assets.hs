module Assets
  ( Assets(..)
  , loadAssets
  ) where

import Graphics.Gloss (Picture, color, rectangleSolid, translate, pictures, makeColorI)
import Graphics.Gloss.Juicy (loadJuicyPNG)
import Codec.Picture (readImage, convertRGBA8, Image(..), PixelRGBA8(..), imageWidth, imageHeight, pixelAt, generateImage, savePngImage, DynamicImage(..))
import Control.Exception (try, SomeException)
import System.FilePath (takeFileName)

-- Assets container
data Assets = Assets
  { aPlayerFrames :: [Picture]
  , aBackground   :: Picture
  , aAlien1Sprite :: Picture
  , aAlien2Sprite :: Picture
  , aAlien3Sprite :: Picture
  , aMenuBackground :: Picture   -- NUEVO
  , aGameOverBackground :: Picture -- NUEVO
  , aGameOverButton   :: Picture   -- NUEVO
  , aHealSmallSprite :: Picture
  , aSpeedBoostSprite :: Picture
  , aDamageBoostSprite :: Picture
  , aDamageText         :: Picture 
  , aSpeedText         :: Picture
  , aBossSprite     :: Picture
  , aAttack1Sprite  :: Picture
  , aAttack2Sprite  :: Picture
  }

-- Wrapper seguro para cargar PNG
tryLoadPNG :: FilePath -> IO (Maybe Picture)
tryLoadPNG path = do
  result <- try (loadJuicyPNG path) :: IO (Either SomeException (Maybe Picture))
  case result of
    Left _ -> return Nothing
    Right val -> return val

-- Carga un JPG o PNG convirtiéndolo a PNG en /tmp/
tryLoadDynamic :: FilePath -> String -> IO (Maybe Picture)
tryLoadDynamic path tempName = do
  result <- readImage path
  case result of
    Left _ -> return Nothing
    Right dyn -> do
      let tempPath = "/tmp/" ++ tempName ++ ".png"
          img = convertRGBA8 dyn
      saveResult <- try (savePngImage tempPath (ImageRGBA8 img)) :: IO (Either SomeException ())
      case saveResult of
        Left _ -> return Nothing
        Right () -> tryLoadPNG tempPath

-- Cortar imagen
cropImage :: Image PixelRGBA8 -> Int -> Int -> Int -> Int -> Image PixelRGBA8
cropImage src x0 y0 tw th = generateImage pick tw th
  where pick i j = pixelAt src (x0 + i) (y0 + j)

-- Función para encontrar el prefijo correcto (./ o ../)
findAssetPrefix :: IO String
findAssetPrefix = do
  let testPath = "assets/player/spaceship.png"
  result <- readImage testPath
  case result of
    Right _ -> return ""          
    Left _  -> return "../"      

-- Load all game assets
loadAssets :: IO Assets
loadAssets = do
  prefix <- findAssetPrefix
  let assetsPath = prefix ++ "assets/"
  
  -- 1. CARGAR NAVE (Spritesheet)
  let shipPath = assetsPath ++ "player/spaceship.png"
  playerDynE <- readImage shipPath
  
  playerFrames <- case playerDynE of
    Left _ -> return []
    Right dyn -> case dyn of
      ImageRGBA8 img -> do
        let frameW = 16; frameH = 16
            coords = [(0, 0), (frameW, 0), (0, frameH), (frameW, frameH)]
            crops = [ cropImage img cx cy frameW frameH | (cx, cy) <- coords ]
            saveAndLoad i crop = do
               let tempPath = "/tmp/ship_frame_" ++ show i ++ ".png"
               _ <- try (savePngImage tempPath (ImageRGBA8 crop)) :: IO (Either SomeException ())
               tryLoadPNG tempPath
        pics <- sequence [ saveAndLoad i c | (i, c) <- zip [1..4] crops ]
        return [ p | Just p <- pics ]
      _ -> return []

  -- 2. FONDOS y Botones
  mBackground <- tryLoadDynamic (assetsPath ++ "background/background_1.jpg") "bg_game_temp"
  mMenuBackground <- tryLoadDynamic (assetsPath ++ "background/menu.jpg") "bg_menu_temp"
  mGameOverBackground <- tryLoadDynamic (assetsPath ++ "background/derrota.jpg") "bg_gameover_temp"
  mGameOverButton <- tryLoadDynamic (assetsPath ++ "background/botonderrota.png") "btn_gameover_temp"
  mDamageText <- tryLoadPNG (assetsPath ++ "stats/damage.png")
  mSpeedText <- tryLoadPNG (assetsPath ++ "stats/speed.png")

  -- 3. ENEMIGOS
  mAlien1Sprite <- tryLoadPNG (assetsPath ++ "enemies/alien1.png")
  mAlien2Sprite <- tryLoadPNG (assetsPath ++ "enemies/alien2.png")
  mAlien3Sprite <- tryLoadPNG (assetsPath ++ "enemies/alien3.png")

  -- 4. ITEMS
  mHealSmallSprite <- tryLoadPNG (assetsPath ++ "items/life.png")
  mSpeedBoostSprite <- tryLoadPNG (assetsPath ++ "items/spd.png")
  mDamageBoostSprite <- tryLoadPNG (assetsPath ++ "items/dmg.png")

  -- 5. BOSS Y ATAQUES
  mBossSprite <- tryLoadPNG (assetsPath ++ "enemies/boss.png")
  mAttack1Sprite <- tryLoadPNG (assetsPath ++ "attacks/at1.png")
  mAttack2Sprite <- tryLoadPNG (assetsPath ++ "attacks/at2.png")

  -- Fallbacks
  let playerFramesFinal = if null playerFrames then [color (makeColorI 0 0 255 255) $ rectangleSolid 16 16] else playerFrames
      backgroundFinal = case mBackground of Just pic -> pic; Nothing -> color (makeColorI 10 10 30 255) $ rectangleSolid 480 360
      menuBackgroundFinal = case mMenuBackground of Just pic -> pic; Nothing -> color (makeColorI 50 0 100 255) $ rectangleSolid 480 360
      gameOverBackgroundFinal = case mGameOverBackground of Just pic -> pic; Nothing -> color (makeColorI 100 0 0 255) $ rectangleSolid 480 360 
      gameOverButtonFinal = case mGameOverButton of Just pic -> pic; Nothing -> color (makeColorI 200 200 200 255) $ rectangleSolid 200 50
      damageTextFinal = case mDamageText of Just pic -> pic; Nothing  -> color (makeColorI 255 255 255 255) $ rectangleSolid 50 20
      speedTextFinal = case mSpeedText of Just pic -> pic; Nothing  -> color (makeColorI 255 255 255 255) $ rectangleSolid 50 20


      alien1SpriteFinal = case mAlien1Sprite of Just pic -> pic; Nothing -> color (makeColorI 255 50 50 255) $ rectangleSolid 24 24
      alien2SpriteFinal = case mAlien2Sprite of Just pic -> pic; Nothing -> color (makeColorI 50 255 50 255) $ rectangleSolid 24 24
      alien3SpriteFinal = case mAlien3Sprite of Just pic -> pic; Nothing -> color (makeColorI 255 100 0 255) $ rectangleSolid 24 24
      
      healSmallSpriteFinal = case mHealSmallSprite of Just pic -> pic; Nothing -> color (makeColorI 0 255 0 255) $ rectangleSolid 16 16
      speedBoostSpriteFinal = case mSpeedBoostSprite of Just pic -> pic; Nothing -> color (makeColorI 0 0 255 255) $ rectangleSolid 16 16
      damageBoostSpriteFinal = case mDamageBoostSprite of Just pic -> pic; Nothing -> color (makeColorI 255 0 255 255) $ rectangleSolid 16 16

      bossSpriteFinal = case mBossSprite of Just pic -> pic; Nothing -> color (makeColorI 128 0 128 255) $ rectangleSolid 64 64
      attack1SpriteFinal = case mAttack1Sprite of Just pic -> pic; Nothing -> color (makeColorI 255 255 0 255) $ rectangleSolid 32 8
      attack2SpriteFinal = case mAttack2Sprite of Just pic -> pic; Nothing -> color (makeColorI 255 128 0 255) $ rectangleSolid 16 16

  return Assets
    { aPlayerFrames   = playerFramesFinal
    , aBackground     = backgroundFinal
    , aAlien1Sprite   = alien1SpriteFinal
    , aAlien2Sprite   = alien2SpriteFinal
    , aAlien3Sprite   = alien3SpriteFinal
    , aMenuBackground = menuBackgroundFinal
    , aGameOverBackground = gameOverBackgroundFinal
    , aGameOverButton     = gameOverButtonFinal
    , aHealSmallSprite    = healSmallSpriteFinal
    , aSpeedBoostSprite   = speedBoostSpriteFinal
    , aDamageBoostSprite  = damageBoostSpriteFinal
    , aDamageText       = damageTextFinal
    , aSpeedText       = speedTextFinal
    , aBossSprite = bossSpriteFinal
    , aAttack1Sprite = attack1SpriteFinal
    , aAttack2Sprite = attack2SpriteFinal
    }