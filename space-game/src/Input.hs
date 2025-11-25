-- Input.hs
module Input
  ( handleEvent
  , updateGameState
  , dirToVector
  ) where

import Graphics.Gloss.Interface.Pure.Game
import GameState
import Enemy
import Wave

-------------------------------------------------------------
-- EVENTOS DE TECLAS
-------------------------------------------------------------

handleEvent :: Event -> GameState -> GameState
handleEvent (EventKey (Char c) keyState _ _) gs = case (c, keyState) of
  ('w', Down) -> addKey DUp gs
  ('a', Down) -> addKey DLeft gs
  ('s', Down) -> addKey DDown gs
  ('d', Down) -> addKey DRight gs
  ('w', Up)   -> removeKey DUp gs
  ('a', Up)   -> removeKey DLeft gs
  ('s', Up)   -> removeKey DDown gs
  ('d', Up)   -> removeKey DRight gs
  (' ', Down) -> shootBullet gs  -- DISPAROS
  _           -> gs

handleEvent (EventKey (SpecialKey KeySpace) Down _ _) gs = shootBullet gs

handleEvent (EventResize newSize) gs =
  gs { windowSize = newSize }

handleEvent _ gs = gs


-- NUEVA FUNCIÓN: DISPAROS ACTUALIZADA
shootBullet :: GameState -> GameState
shootBullet gs@GameState{ playerPos = pPos, playerDir = pDir, bullets = bs, playerDamage = pDmg } =
  let 
    initialBulletSpeed = 400.0
    newBullet = Bullet { 
        bulletPos    = pPos, 
        bulletDir    = pDir, 
        bulletSpeed  = initialBulletSpeed,
        bulletDamage = pDmg -- La bala hereda el daño actual del jugador
    }
  in gs { bullets = newBullet : bs }

-------------------------------------------------------------
-- UPDATE GENERAL DEL JUEGO
-------------------------------------------------------------

updateGameState :: Float -> GameState -> GameState
updateGameState dt =
      updatePlayer dt

-------------------------------------------------------------
-- SISTEMA DE MOVIMIENTO DEL JUGADOR
-------------------------------------------------------------

updatePlayer :: Float -> GameState -> GameState
updatePlayer dt gs = gs
  { playerPos = newPos
  , playerDir = newDir
  , animTime = animTime gs + dt
  }
  where
    keys = keysDown gs
    (dx, dy) = foldr addVector (0, 0) (map dirToVector keys)
    
    newDir = if null keys
             then playerDir gs
             else head keys

    magnitude = sqrt (dx*dx + dy*dy)
    (ndx, ndy) = if magnitude > 0 then (dx/magnitude, dy/magnitude) else (0,0)

    speedActual = currentSpeed gs

    moveX = ndx * speedActual * dt
    moveY = ndy * speedActual * dt

    (px, py) = playerPos gs

    terrainW = 480.0
    terrainH = 360.0
    minX = -terrainW / 2
    maxX = terrainW / 2
    minY = -terrainH / 2
    maxY = terrainH / 2

    newX = clamp (px + moveX) minX maxX
    newY = clamp (py + moveY) minY maxY
    newPos = (newX, newY)

    addVector (a,b) (c,d) = (a+c, b+d)

-------------------------------------------------------------
-- UTILIDADES
-------------------------------------------------------------

dirToVector :: Direction -> (Float, Float)
dirToVector DUp    = (0, 1)
dirToVector DDown  = (0,-1)
dirToVector DLeft  = (-1,0)
dirToVector DRight = (1, 0)

clamp :: Float -> Float -> Float -> Float
clamp x a b = max a (min b x)