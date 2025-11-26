module Assets
  ( Assets(..)
  , loadAssets
  ) where

import Graphics.Gloss (Picture, color, rectangleSolid, translate, pictures, makeColorI)
import Graphics.Gloss.Juicy (loadJuicyPNG)
import Codec.Picture (readImage, convertRGBA8, Image(..), PixelRGBA8(..), imageWidth, imageHeight, pixelAt, generateImage, savePngImage, DynamicImage(..))
import Control.Exception (try, SomeException)

-- Assets container
data Assets = Assets
  { aPlayerFrames   :: [Picture]
  , aBackground     :: Picture
  , aAlien1Sprite   :: Picture
  , aAlien2Sprite   :: Picture
  , aAlien3Sprite   :: Picture
  , aMenuBackground :: Picture
  , aHealSmallSprite :: Picture
  , aSpeedBoostSprite :: Picture
  , aDamageBoostSprite :: Picture
  }

-- Wrapper seguro para cargar PNG
tryLoadPNG :: FilePath -> IO (Maybe Picture)
tryLoadPNG path = do
  result <- try (loadJuicyPNG path) :: IO (Either SomeException (Maybe Picture))
  case result of
    Left _ -> return Nothing
    Right val -> return val

-- Carga un JPG convirtiéndolo a PNG en /tmp/
tryLoadJPG :: FilePath -> String -> IO (Maybe Picture)
tryLoadJPG path tempName = do
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
    Right _ -> return ""          -- Está en la carpeta actual
    Left _  -> return "../"      -- Probablemente está arriba

-- Load all game assets
loadAssets :: IO Assets
loadAssets = do
  prefix <- findAssetPrefix
  let assetsPath = prefix ++ "assets/"
  
  putStrLn $ "Buscando assets en: " ++ assetsPath

  -- 1. CARGAR NAVE (Spritesheet)
  let shipPath = assetsPath ++ "player/spaceship.png"
  playerDynE <- readImage shipPath
  
  playerFrames <- case playerDynE of
    Left err -> do
      putStrLn $ "Error leyendo nave (" ++ shipPath ++ "): " ++ err
      return []
    Right dyn -> case dyn of
      ImageRGBA8 img -> do
        let frameW = 16
            frameH = 16
            coords = [(0, 0), (frameW, 0), (0, frameH), (frameW, frameH)]
            crops = [ cropImage img cx cy frameW frameH | (cx, cy) <- coords ]
            
            -- Guardar frames en /tmp/ para evitar problemas de permisos locales
            saveAndLoad i crop = do
               let tempPath = "/tmp/ship_frame_" ++ show i ++ ".png"
               _ <- try (savePngImage tempPath (ImageRGBA8 crop)) :: IO (Either SomeException ())
               tryLoadPNG tempPath

        pics <- sequence [ saveAndLoad i c | (i, c) <- zip [1..4] crops ]
        return [ p | Just p <- pics ]
      _ -> return []

  -- 2. CARGAR FONDO JUEGO
  mBackground <- tryLoadJPG (assetsPath ++ "background/background_1.jpg") "bg_game_temp"

  -- 3. CARGAR FONDO MENU
  mMenuBackground <- tryLoadJPG (assetsPath ++ "background/menu.jpg") "bg_menu_temp"

  -- 4. CARGAR ENEMIGO ALIEN1
  putStrLn $ "Cargando alien1 desde: " ++ assetsPath ++ "enemies/alien1.png"
  mAlien1Sprite <- tryLoadPNG (assetsPath ++ "enemies/alien1.png")
  case mAlien1Sprite of
    Just _ -> putStrLn "✓ Alien1 cargado exitosamente"
    Nothing -> putStrLn "✗ ERROR: No se pudo cargar alien1.png"

  -- 4b. CARGAR ENEMIGO ALIEN2
  putStrLn $ "Cargando alien2 desde: " ++ assetsPath ++ "enemies/alien2.png"
  mAlien2Sprite <- tryLoadPNG (assetsPath ++ "enemies/alien2.png")
  case mAlien2Sprite of
    Just _ -> putStrLn "✓ Alien2 cargado exitosamente"
    Nothing -> putStrLn "✗ ERROR: No se pudo cargar alien2.png"

  -- 4c. CARGAR ENEMIGO ALIEN3 (KAMIKAZE)
  putStrLn $ "Cargando alien3 desde: " ++ assetsPath ++ "enemies/alien3.png"
  mAlien3Sprite <- tryLoadPNG (assetsPath ++ "enemies/alien3.png")
  case mAlien3Sprite of
    Just _ -> putStrLn "✓ Alien3 cargado exitosamente"
    Nothing -> putStrLn "✗ ERROR: No se pudo cargar alien3.png"

  -- 5. CARGAR ITEMS
  putStrLn $ "Cargando items desde: " ++ assetsPath ++ "items/"
  mHealSmallSprite <- tryLoadPNG (assetsPath ++ "items/life.png")
  case mHealSmallSprite of
    Just _ -> putStrLn "✓ Life item cargado exitosamente"
    Nothing -> putStrLn "✗ ERROR: No se pudo cargar life.png"
    
  mSpeedBoostSprite <- tryLoadPNG (assetsPath ++ "items/spd.png")
  case mSpeedBoostSprite of
    Just _ -> putStrLn "✓ Speed item cargado exitosamente"
    Nothing -> putStrLn "✗ ERROR: No se pudo cargar spd.png"
    
  mDamageBoostSprite <- tryLoadPNG (assetsPath ++ "items/dmg.png")
  case mDamageBoostSprite of
    Just _ -> putStrLn "✓ Damage item cargado exitosamente"
    Nothing -> putStrLn "✗ ERROR: No se pudo cargar dmg.png"

  -- Fallbacks (Cuadrados de color si falla la carga)
  let playerFramesFinal = if null playerFrames 
                          then [color (makeColorI 0 0 255 255) $ rectangleSolid 16 16] 
                          else playerFrames
      
      backgroundFinal = case mBackground of
        Just pic -> pic
        Nothing  -> color (makeColorI 10 10 30 255) $ rectangleSolid 480 360

      menuBackgroundFinal = case mMenuBackground of
        Just pic -> pic
        Nothing  -> color (makeColorI 50 0 100 255) $ rectangleSolid 480 360

      alien1SpriteFinal = case mAlien1Sprite of
        Just pic -> pic
        Nothing  -> color (makeColorI 255 50 50 255) $ rectangleSolid 24 24

      alien2SpriteFinal = case mAlien2Sprite of
        Just pic -> pic
        Nothing  -> color (makeColorI 50 255 50 255) $ rectangleSolid 24 24

      alien3SpriteFinal = case mAlien3Sprite of
        Just pic -> pic
        Nothing  -> color (makeColorI 255 100 0 255) $ rectangleSolid 24 24

      healSmallSpriteFinal = case mHealSmallSprite of
        Just pic -> pic
        Nothing  -> color (makeColorI 0 255 0 255) $ rectangleSolid 16 16

      speedBoostSpriteFinal = case mSpeedBoostSprite of
        Just pic -> pic
        Nothing  -> color (makeColorI 0 0 255 255) $ rectangleSolid 16 16

      damageBoostSpriteFinal = case mDamageBoostSprite of
        Just pic -> pic
        Nothing  -> color (makeColorI 255 0 255 255) $ rectangleSolid 16 16

  return Assets
    { aPlayerFrames   = playerFramesFinal
    , aBackground     = backgroundFinal
    , aAlien1Sprite   = alien1SpriteFinal
    , aAlien2Sprite   = alien2SpriteFinal
    , aAlien3Sprite   = alien3SpriteFinal
    , aMenuBackground = menuBackgroundFinal
    , aHealSmallSprite = healSmallSpriteFinal
    , aSpeedBoostSprite = speedBoostSpriteFinal
    , aDamageBoostSprite = damageBoostSpriteFinal
    }