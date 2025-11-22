-- GameState.hs
-- Define the game state and player data
module GameState
  ( GameState(..)
  , Direction(..)
  , initialState
  , playerSize
  , playerSpeed
  ) where

-- Directions for player movement
data Direction = DUp | DDown | DLeft | DRight
  deriving (Eq, Show)

-- Game state containing all game data
data GameState = GameState
  { playerPos   :: (Float, Float)  -- Player position (x, y)
  , keysDown    :: [Direction]     -- Currently pressed keys
  , animTime    :: Float           -- Animation timer for sprite cycling
  , windowSize  :: (Int, Int)      -- Window dimensions
  } deriving (Show)

-- Player sprite size (16x16 pixels)
playerSize :: Float
playerSize = 16.0

-- Player movement speed (pixels per second)
playerSpeed :: Float
playerSpeed = 200.0

-- Initial game state
initialState :: GameState
initialState = GameState
  { playerPos  = (400, 300)  -- Start in center (will be adjusted based on window)
  , keysDown   = []
  , animTime   = 0.0
  , windowSize = (800, 600)
  }
