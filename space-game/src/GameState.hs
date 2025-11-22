module GameState
  ( GameState(..)
  , Direction(..)
  , initialState
  , playerSize
  , playerSpeed
  , addKey
  , removeKey
  ) where

import Enemy
import Wave

-------------------------------------------------------------
-- DIRECCIONES
-------------------------------------------------------------

data Direction = DUp | DDown | DLeft | DRight
  deriving (Eq, Show)

-------------------------------------------------------------
-- GAMESTATE
-------------------------------------------------------------

data GameState = GameState
  { playerPos   :: (Float, Float)
  , keysDown    :: [Direction]
  , animTime    :: Float
  , windowSize  :: (Int, Int)
  , enemies     :: [Enemy]
  , wave        :: Wave
  } deriving (Show)

-------------------------------------------------------------
-- CONSTANTES
-------------------------------------------------------------

playerSize :: Float
playerSize = 16.0

playerSpeed :: Float
playerSpeed = 200.0

-------------------------------------------------------------
-- ESTADO INICIAL
-------------------------------------------------------------

initialState :: GameState
initialState = GameState
  { playerPos   = (0, 0)
  , keysDown    = []
  , animTime    = 0.0
  , windowSize  = (800, 600)
  , enemies     = []
  , wave        = initialWave
  }

-------------------------------------------------------------
-- FUNCIONES PARA GESTIONAR INPUT (keys)
-------------------------------------------------------------

addKey :: Direction -> GameState -> GameState
addKey dir gs =
  gs { keysDown =
        if dir `elem` keysDown gs
        then keysDown gs
        else dir : keysDown gs }

removeKey :: Direction -> GameState -> GameState
removeKey dir gs =
  gs { keysDown = filter (/= dir) (keysDown gs) }
