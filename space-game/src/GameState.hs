module GameState
  ( GameState(..)
  , Direction(..)
  , Bullet(..)
  , initialState
  , playerSize
  , basePlayerSpeed
  , addKey
  , removeKey
  ) where

import Enemy
import Wave
import Item (Item)

-------------------------------------------------------------
-- DIRECCIONES
-------------------------------------------------------------

data Direction = DUp | DDown | DLeft | DRight
  deriving (Eq, Show)

-------------------------------------------------------------
-- PROYECTIL
-------------------------------------------------------------

data Bullet = Bullet
  { bulletPos    :: (Float, Float)
  , bulletDir    :: Direction
  , bulletSpeed  :: Float
  , bulletDamage :: Int  -- NUEVO: La bala lleva el daño con el que fue disparada
  } deriving (Show)  

-------------------------------------------------------------
-- GAMESTATE
-------------------------------------------------------------

data GameState = GameState
  { playerPos    :: (Float, Float)
  , playerDir    :: Direction 
  , keysDown     :: [Direction]
  , animTime     :: Float
  , windowSize   :: (Int, Int)
  
  -- Entidades
  , enemies      :: [Enemy]
  , bullets      :: [Bullet]
  , items        :: [Item]
  
  -- Estado del Juego
  , wave         :: Wave
  , waveCount    :: Int
  , playerHP     :: Int          
  , currentSpeed :: Float        
  , playerDamage :: Int          -- NUEVO: Reemplaza a 'score'. Daño actual del jugador.
  } deriving (Show)

-------------------------------------------------------------
-- CONSTANTES
-------------------------------------------------------------

playerSize :: Float
playerSize = 16.0

basePlayerSpeed :: Float
basePlayerSpeed = 200.0

-------------------------------------------------------------
-- ESTADO INICIAL
-------------------------------------------------------------

initialState :: GameState
initialState = GameState
  { playerPos    = (0, 0)
  , playerDir    = DUp
  , keysDown     = []
  , animTime     = 0.0
  , windowSize   = (480, 360)
  , enemies      = []
  , bullets      = []
  , items        = []            
  , wave         = initialWave
  , waveCount    = 1
  , playerHP     = 100           
  , currentSpeed = basePlayerSpeed
  , playerDamage = 10            -- Daño inicial (empiezas pegando 10)
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