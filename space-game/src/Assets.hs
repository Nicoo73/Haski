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
  , aCazaSprite     :: Picture
  , aMenuBackground :: Picture
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

  -- 4. CARGAR ENEMIGO CAZA
  mCazaSprite <- tryLoadPNG (assetsPath ++ "enemies/caza.png")

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

      cazaSpriteFinal = case mCazaSprite of
        Just pic -> pic
        Nothing  -> color (makeColorI 255 50 50 255) $ rectangleSolid 24 24

  return Assets
    { aPlayerFrames   = playerFramesFinal
    , aBackground     = backgroundFinal
    , aCazaSprite     = cazaSpriteFinal
    , aMenuBackground = menuBackgroundFinal
    }