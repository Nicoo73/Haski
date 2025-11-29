module GameState
  ( GameState(..)
  , Direction(..)
  , Bullet(..)
  , EnemyBullet(..)
  , PlayerStats(..)
  , Wave(..)
  , ScreenState(..) -- ¡Asegúrate de exportar esto!
  , initialState
  , initialWave
  , playerSize
  , playerStats
  , playerRange
  , playerPickupRange
  , terrainWidth
  , terrainHeight
  , addKey
  , removeKey
  ) where

import Enemy (Direction(..))
import qualified Enemy as E
import Item (Item)
import qualified Boss as B

-------------------------------------------------------------
-- TIPOS DE PANTALLA
-------------------------------------------------------------

-- ¡AQUÍ ESTABA EL ERROR! Faltaba 'GameOver'
data ScreenState = Menu | Playing | GameOver
  deriving (Eq, Show)

-------------------------------------------------------------
-- ESTADÍSTICAS DEL JUGADOR
-------------------------------------------------------------

data PlayerStats = PlayerStats
  { playerHealth       :: Int
  , playerDamage       :: Int
  , playerMoveSpeed    :: Float  
  , playerBulletSpeed  :: Float  
  , playerDamageBonus  :: Int    
  , playerSpeedBonus   :: Float  
  } deriving (Show)

-------------------------------------------------------------
-- PROYECTIL DEL JUGADOR
-------------------------------------------------------------

data Bullet = Bullet
  { bulletPos :: (Float, Float)
  , bulletDir :: Direction
  , bulletSpeed :: Float
  , bulletDamage :: Int 
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
-- WAVE 
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
  { currentScreen  :: ScreenState
  , playerPos      :: (Float, Float)
  , playerDir      :: Direction 
  , keysDown       :: [Direction]
  , animTime       :: Float
  , windowSize     :: (Int, Int)
  , enemies        :: [E.Enemy]
  , bullets        :: [Bullet]
  , enemyBullets   :: [EnemyBullet]
  , items          :: [Item]       
  , wave           :: Wave
  , waveCount      :: Int
  , currentHealth  :: Int          
  , currentStats   :: PlayerStats
  , maybeBoss      :: Maybe B.Boss
  , bossAttacks    :: [B.BossAttack]
  , bossSpawned    :: Bool
  , bossSpawnTimer :: Float  -- Timer para delay de 3s antes de spawnar boss
  } deriving (Show)

-------------------------------------------------------------
-- CONSTANTES
-------------------------------------------------------------

playerSize :: Float
playerSize = 16.0

playerPickupRange :: Float
playerPickupRange = 25.0 

terrainWidth :: Float
terrainWidth = 640.0  

terrainHeight :: Float
terrainHeight = 360.0

playerStats :: PlayerStats
playerStats = PlayerStats
  { playerHealth      = 100
  , playerDamage      = 45
  , playerMoveSpeed   = 200.0 
  , playerBulletSpeed = 225.0 
  , playerDamageBonus = 0      
  , playerSpeedBonus  = 0.0    
  }

playerRange :: Float
playerRange = min terrainWidth terrainHeight * 0.7 

-------------------------------------------------------------
-- ESTADO INICIAL
-------------------------------------------------------------

initialState :: GameState
initialState = GameState
  { currentScreen = Menu -- Inicia en el menú
  , playerPos     = (0, 0)
  , playerDir     = DUp
  , keysDown      = []
  , animTime      = 0.0
  , windowSize    = (640, 360)  
  , enemies       = []
  , bullets       = []
  , enemyBullets  = []
  , items         = []         
  , wave          = initialWave
  , waveCount     = 1
  , currentHealth = playerHealth playerStats
  , currentStats  = playerStats
  , maybeBoss     = Nothing
  , bossAttacks   = []
  , bossSpawned   = False
  , bossSpawnTimer = 0.0
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