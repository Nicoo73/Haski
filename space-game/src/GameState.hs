module GameState
  ( GameState(..)
  , Direction(..)
  , Bullet(..)
  , initialState
  , playerSize
  , playerSpeed
  --, bulletSpeed
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
-- PROYECTIL
-------------------------------------------------------------

--bulletSpeed :: Float
--bulletSpeed = 400.0

data Bullet = Bullet
  { bulletPos :: (Float, Float)
  , bulletDir :: Direction
  , bulletSpeed ::Float
  } deriving (Show)  

-------------------------------------------------------------
-- GAMESTATE
-------------------------------------------------------------

data GameState = GameState
  { playerPos   :: (Float, Float)
  , playerDir   :: Direction 
  , keysDown    :: [Direction]
  , animTime    :: Float
  , windowSize  :: (Int, Int)
  , enemies     :: [Enemy]
  , bullets     :: [Bullet]
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
  , playerDir   = DUp
  , keysDown    = []
  , animTime    = 0.0
  , windowSize  = (480, 360)
  , enemies     = []
  , bullets     = []
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
