module Item
  ( Item(..)
  , ItemType(..)
  , spawnItemPure
  , spawnItemForEnemy
  ) where

import Enemy (EnemyType(..))

-------------------------------------------------------------
-- 1. TIPOS DE ÍTEM
-------------------------------------------------------------

data ItemType
  = HealSmall       -- +20 HP o +15 HP máximo si está full
  | SpeedBoost      -- +Velocidad (Color Azul)
  | DamageBoost     -- +Daño (Color Violeta/Rojo)
  deriving (Show, Eq)

-------------------------------------------------------------
-- 2. ÍTEM EN EL MUNDO
-------------------------------------------------------------

data Item = Item
  { itemType :: ItemType
  , itemPos  :: (Float, Float)
  , itemRadius :: Float
  } deriving (Show)

-------------------------------------------------------------
-- 3. SPAWNEO POR TIPO DE ENEMIGO
-------------------------------------------------------------

-- Genera item según el tipo de enemigo con probabilidades específicas
spawnItemForEnemy :: EnemyType -> (Float, Float) -> Maybe Item
spawnItemForEnemy enemyType (x, y) =
  let 
    -- "Hash" simple de las coordenadas para simular aleatoriedad
    seed = floor (abs (x * 37 + y * 13)) `mod` 100 
    
    mkItem t = Item { itemType = t, itemPos = (x,y), itemRadius = 10.0 } 
  in 
    case enemyType of
      Alien1 -> if seed < 51 then Just (mkItem DamageBoost) else Nothing  -- 51% daño
      Alien2 -> if seed < 50 then Just (mkItem HealSmall) else Nothing    -- 50% vida
      Alien3 -> if seed < 40 then Just (mkItem SpeedBoost) else Nothing   -- 40% velocidad

-------------------------------------------------------------
-- 4. SPAWNEO PURO (Para compatibilidad)
-------------------------------------------------------------

-- Decide determinísticamente si spawnea un item basado en las coordenadas
spawnItemPure :: (Float, Float) -> Maybe Item
spawnItemPure (x, y) =
  let 
    seed = floor (abs (x * 37 + y * 13)) `mod` 100 
    mkItem t = Item { itemType = t, itemPos = (x,y), itemRadius = 10.0 } 
  in 
    if seed < 20 then Just (mkItem HealSmall)
    else if seed < 30 then Just (mkItem SpeedBoost)
    else if seed < 40 then Just (mkItem DamageBoost)
    else Nothing