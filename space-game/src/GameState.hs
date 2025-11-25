module GameState
  ( GameState(..)
  , Direction(..)
  , Bullet(..)
  , ScreenState(..)
  , initialState
  , playerSize
  , playerSpeed
  , addKey
  , removeKey
  ) where

import Enemy
import Wave

-------------------------------------------------------------
-- TIPOS DE PANTALLA
-------------------------------------------------------------

data ScreenState = Menu | Playing | GameOver -- Asegúrate de que esto esté así
  deriving (Eq, Show)

-------------------------------------------------------------
-- DIRECCIONES
-------------------------------------------------------------

data Direction = DUp | DDown | DLeft | DRight 
               | DUpLeft | DUpRight | DDownLeft | DDownRight
  deriving (Eq, Show)


-------------------------------------------------------------
-- PROYECTIL
-------------------------------------------------------------

data Bullet = Bullet
  { bulletPos   :: (Float, Float)
  , bulletDir   :: Direction
  , bulletSpeed :: Float
  } deriving (Show)  

-------------------------------------------------------------
-- GAMESTATE
-------------------------------------------------------------

data GameState = GameState
  { currentScreen :: ScreenState
  , playerPos   :: (Float, Float)
  , playerDir   :: Direction 
  , playerHealth :: Int
  , keysDown    :: [Direction]
  , animTime    :: Float
  , windowSize  :: (Int, Int)
  , enemies     :: [Enemy]
  , bullets     :: [Bullet]
  , wave        :: Wave
  , waveCount   :: Int
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
  { currentScreen = Menu
  , playerPos   = (0, 0)
  , playerDir   = DUp
  , playerHealth = 100
  , keysDown    = []
  , animTime    = 0.0
  , windowSize  = (480, 360)
  , enemies     = []
  , bullets     = []
  , wave        = initialWave
  , waveCount   = 1
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