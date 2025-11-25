module Item
  ( Item(..)
  , ItemType(..)
  , spawnItemPure
  ) where

-------------------------------------------------------------
-- 1. TIPOS DE ÍTEM
-------------------------------------------------------------

data ItemType
  = HealSmall       -- +10 HP (Color Verde)
  | SpeedBoost      -- +Velocidad (Color Azul)
  | DamageBoost     -- +Daño (Color Violeta/Rojo) - REEMPLAZA AL SCORE
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
-- 3. SPAWNEO PURO (Pseudo-aleatorio basado en posición)
-------------------------------------------------------------

-- Decide determinísticamente si spawnea un item basado en las coordenadas
spawnItemPure :: (Float, Float) -> Maybe Item
spawnItemPure (x, y) =
  let 
    -- "Hash" simple de las coordenadas para simular aleatoriedad
    seed = floor (abs (x * 37 + y * 13)) `mod` 100 
    
    mkItem t = Item { itemType = t, itemPos = (x,y), itemRadius = 8.0 }
  in 
    if seed < 20 then Just (mkItem HealSmall)       -- 20% probabilidad
    else if seed < 30 then Just (mkItem SpeedBoost)  -- 10% probabilidad
    else if seed < 40 then Just (mkItem DamageBoost) -- 10% probabilidad (Antes Score)
    else Nothing                                     -- 60% nada