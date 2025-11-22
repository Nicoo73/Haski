-- Assets.hs
-- Load and manage game assets (sprites, backgrounds)
module Assets
  ( Assets(..)
  , loadAssets
  ) where

import Graphics.Gloss (Picture, color, rectangleSolid, translate, pictures, makeColorI, rotate)
import Graphics.Gloss.Juicy (loadJuicyPNG)
import Codec.Picture (readImage, convertRGBA8, Image(..), PixelRGBA8(..), imageWidth, imageHeight, pixelAt, generateImage, savePngImage, DynamicImage(..))

-- Assets container holding all loaded images
data Assets = Assets
  { aPlayerFrames :: [Picture]  -- 4 frames of player sprite (16x16 each)
  , aBackground   :: Picture    -- Background image
  }

-- Try to load a PNG file
tryLoad :: FilePath -> IO (Maybe Picture)
tryLoad = loadJuicyPNG

-- Try to load a JPG file by converting to PNG format first
tryLoadJPG :: FilePath -> IO (Maybe Picture)
tryLoadJPG path = do
  result <- readImage path
  case result of
    Left _ -> return Nothing
    Right dyn -> case dyn of
      ImageRGBA8 img -> do
        let tempPath = path ++ ".temp.png"
        savePngImage tempPath (ImageRGBA8 img)
        pic <- tryLoad tempPath
        return pic
      _ -> do
        let img = convertRGBA8 dyn
            tempPath = path ++ ".temp.png"
        savePngImage tempPath (ImageRGBA8 img)
        pic <- tryLoad tempPath
        return pic

-- Crop a region from an image
cropImage :: Image PixelRGBA8 -> Int -> Int -> Int -> Int -> Image PixelRGBA8
cropImage src x0 y0 tw th = generateImage pick tw th
  where
    pick i j = pixelAt src (x0 + i) (y0 + j)

-- Load all game assets
loadAssets :: IO Assets
loadAssets = do
  -- Load player sprite sheet (32x32 containing 4 frames of 16x16)
  playerDynE <- readImage "../assets/player/spaceship.png"
  playerFrames <- case playerDynE of
    Left err -> do
      putStrLn $ "Warning: Could not load player sprite: " ++ err
      return []
    Right dyn -> case dyn of
      ImageRGBA8 img -> do
        let w = imageWidth img
            h = imageHeight img
            frameW = 16
            frameH = 16
            -- Coordinates for 4 quadrants: top-left, top-right, bottom-left, bottom-right
            coords = [(0, 0), (frameW, 0), (0, frameH), (frameW, frameH)]
            frameFiles = [ "../assets/player/spaceship_" ++ show i ++ ".png" | i <- [1..4] ]
            crops = [ cropImage img cx cy frameW frameH | (cx, cy) <- coords ]
        -- Save cropped frames as temporary files
        sequence_ [ savePngImage f (ImageRGBA8 c) | (f, c) <- zip frameFiles crops ]
        -- Load the cropped frames
        pics <- mapM tryLoad frameFiles
        return [ p | Just p <- pics ]
      _ -> do
        putStrLn "Warning: Player sprite is not RGBA8 format"
        return []

  -- Load background (JPG format)
  mBackground <- tryLoadJPG "../assets/background/background_1.jpg"

  -- Create fallback graphics if loading fails
  let playerFramesFinal = if null playerFrames
                          then [ translate 0 0 $ color (makeColorI 100 100 255 255) $ rectangleSolid 16 16 ]
                          else playerFrames
      
      backgroundFinal = case mBackground of
        Just pic -> pic
        Nothing  -> color (makeColorI 10 10 30 255) $ rectangleSolid 480 360

  return Assets
    { aPlayerFrames = playerFramesFinal
    , aBackground   = backgroundFinal
    }
