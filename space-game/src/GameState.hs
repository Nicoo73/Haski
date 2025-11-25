module GameState
  ( GameState(..)
  , Direction(..)  -- Re-export from Enemy
  , Bullet(..)
  , EnemyBullet(..)
  , PlayerStats(..)
  , Wave(..)
  , initialState
  , initialWave
  , playerSize
  , playerStats
  , playerRange
  , terrainWidth
  , terrainHeight
  , addKey
  , removeKey
  ) where

import Enemy (Direction(..))
import qualified Enemy as E
-- No importar Wave aquí para evitar dependencia circular
-- Wave importará GameState solo para PlayerStats

-------------------------------------------------------------
-- DIRECCIONES (Re-exportado desde Enemy)
-------------------------------------------------------------
-- Definición movida a Enemy.hs para evitar dependencia circular


-------------------------------------------------------------
-- ESTADÍSTICAS DEL JUGADOR
-------------------------------------------------------------

data PlayerStats = PlayerStats
  { playerHealth       :: Int
  , playerDamage       :: Int
  , playerMoveSpeed    :: Float  -- Base movement speed
  , playerBulletSpeed  :: Float  -- Base bullet speed
  } deriving (Show)

-------------------------------------------------------------
-- PROYECTIL DEL JUGADOR
-------------------------------------------------------------

data Bullet = Bullet
  { bulletPos :: (Float, Float)
  , bulletDir :: Direction
  , bulletSpeed :: Float
  } deriving (Show)

-------------------------------------------------------------
-- PROYECTIL DEL ENEMIGO
-------------------------------------------------------------

data EnemyBullet = EnemyBullet
  { eBulletPos :: (Float, Float)
  , eBulletDir :: Direction
  , eBulletSpeed :: Float
  , eBulletDamage :: Int
  } deriving (Show)

-------------------------------------------------------------
-- WAVE (definición local para evitar dependencia circular)
-------------------------------------------------------------

data Wave = Wave
  { enemiesLeft    :: Int
  , timeSinceSpawn :: Float
  } deriving (Show)

initialWave :: Wave
initialWave = Wave { enemiesLeft = 5, timeSinceSpawn = 0 }  

-------------------------------------------------------------
-- GAMESTATE
-------------------------------------------------------------

data GameState = GameState
  { playerPos      :: (Float, Float)
  , playerDir      :: Direction 
  , keysDown       :: [Direction]
  , animTime       :: Float
  , windowSize     :: (Int, Int)
  , enemies        :: [E.Enemy]
  , bullets        :: [Bullet]
  , enemyBullets   :: [EnemyBullet]
  , wave           :: Wave
  , waveCount      :: Int
  , currentHealth  :: Int  -- Vida actual del jugador
  } deriving (Show)

-------------------------------------------------------------
-- CONSTANTES
-------------------------------------------------------------

playerSize :: Float
playerSize = 16.0

-- Dimensiones del terreno
terrainWidth :: Float
terrainWidth = 640.0  -- Aumentado para dar más espacio horizontal

terrainHeight :: Float
terrainHeight = 360.0

-- Estadísticas base del jugador (configurables)
playerStats :: PlayerStats
playerStats = PlayerStats
  { playerHealth      = 10
  , playerDamage      = 2
  , playerMoveSpeed   = 200.0  -- Pixels per second
  , playerBulletSpeed = 400.0  -- Pixels per second
  }

-- Rango efectivo del jugador (70% del terreno disponible)
playerRange :: Float
playerRange = min terrainWidth terrainHeight * 0.7  -- 70% del lado más pequeño

-------------------------------------------------------------
-- ESTADO INICIAL
-------------------------------------------------------------

initialState :: GameState
initialState = GameState
  { playerPos     = (0, 0)
  , playerDir     = DUp
  , keysDown      = []
  , animTime      = 0.0
  , windowSize    = (640, 360)  -- Actualizado al nuevo tamaño de terreno
  , enemies       = []
  , bullets       = []
  , enemyBullets  = []
  , wave          = initialWave
  , waveCount     = 1
  , currentHealth = playerHealth playerStats
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
