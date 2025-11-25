-- Input.hs
-- Handle keyboard input, player movement, and enemy/wave updates

module Input
  ( handleEvent
  , updateGameState
  , dirToVector
  ) where

import Graphics.Gloss.Interface.Pure.Game
import GameState
import Enemy
import Wave

------------------------------------------------------------
-- CONSTANTES DEL BOTÓN (Para detección de clics)
-------------------------------------------------------------

-- Posición del botón (centro del mapa, debe coincidir con Render.hs)
buttonX, buttonY :: Float
buttonX = 0.0
buttonY = -50.0

-- Tamaño del botón (debe coincidir con Render.hs)
buttonW, buttonH :: Float
buttonW = 250.0
buttonH = 60.0


-------------------------------------------------------------
-- EVENTOS DE TECLAS
-------------------------------------------------------------

handleEvent :: Event -> GameState -> GameState
handleEvent (EventKey (MouseButton LeftButton) Down _ (mx, my)) gs 
  | currentScreen gs == Menu = handleMenuClick mx my gs
handleEvent (EventKey (Char c) keyState _ _) gs 
  | currentScreen gs == Playing = case (c, keyState) of
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

-- ¡ESTA LÍNEA DEBE EXISTIR SI OCURRE UN FALLO SILENCIOSO EN LA DETECCIÓN!
handleEvent (EventKey (SpecialKey KeySpace) Down _ _) gs = shootBullet gs

handleEvent (EventResize newSize) gs =
  gs { windowSize = newSize }

handleEvent _ gs = gs


------------------------------------------------------------
-- LÓGICA DE MENÚ
-------------------------------------------------------------

handleMenuClick :: Float -> Float -> GameState -> GameState
handleMenuClick mx my gs
  | isInsideButton mx my = gs { currentScreen = Playing }
  | otherwise = gs
  where
    -- Función para detectar si el clic está dentro del área del botón
    isInsideButton x y =
      x >= buttonX - buttonW / 2 &&
      x <= buttonX + buttonW / 2 &&
      y >= buttonY - buttonH / 2 &&
      y <= buttonY + buttonH / 2



-- NUEVA FUNCIÓN: DISPAROS
shootBullet :: GameState -> GameState
shootBullet gs@GameState{ playerPos = pPos, playerDir = pDir, bullets = bs } =
  let 
    initialBulletSpeed = 400.0 -- Define la velocidad base aquí
    newBullet = Bullet { 
        bulletPos = pPos, 
        bulletDir = pDir, 
        bulletSpeed = initialBulletSpeed -- ¡ASIGNAR VALOR!
    }
  in gs { bullets = newBullet : bs }

-------------------------------------------------------------
-- UPDATE GENERAL DEL JUEGO
-------------------------------------------------------------

updateGameState :: Float -> GameState -> GameState
updateGameState dt =
      updatePlayer dt       -- mover jugador
  .   updateBullets dt      -- mover proyectiles
 --   . updateEnemies dt      -- mover enemigos
 -- . updateWave dt         -- spawnear enemigos
   

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
    
    -- Determine facing direction based on movement vector (8 directions)
    newDir = if dx == 0 && dy == 0
             then playerDir gs  -- No movement, keep current direction
             else vectorToDirection (dx, dy)

    magnitude = sqrt (dx*dx + dy*dy)
    (ndx, ndy) = if magnitude > 0 then (dx/magnitude, dy/magnitude) else (0,0)

    moveX = ndx * playerSpeed * dt
    moveY = ndy * playerSpeed * dt

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
-- SISTEMA DE PROYECTILES
-------------------------------------------------------------

updateBullets :: Float -> GameState -> GameState
updateBullets dt gs@GameState{ bullets = bs } =
  gs { bullets = filter inBounds (map (moveBullet dt) bs) }
  where
    -- Move bullet based on its direction and speed
    moveBullet :: Float -> Bullet -> Bullet
    moveBullet deltaTime bullet =
      let (bx, by) = bulletPos bullet
          dir = bulletDir bullet
          speed = bulletSpeed bullet
          (dx, dy) = dirToVector dir
          newX = bx + dx * speed * deltaTime
          newY = by + dy * speed * deltaTime
      in bullet { bulletPos = (newX, newY) }
    
    -- Check if bullet is still within terrain bounds
    inBounds :: Bullet -> Bool
    inBounds bullet =
      let (bx, by) = bulletPos bullet
          terrainW = 480.0
          terrainH = 360.0
          margin = 50.0  -- Extra margin before removing bullet
      in abs bx < (terrainW / 2 + margin) && abs by < (terrainH / 2 + margin)

-------------------------------------------------------------
-- SISTEMA DE ENEMIGOS (con Enemy.hs moderno)
-------------------------------------------------------------

--updateEnemies :: Float -> GameState -> GameState
--updateEnemies dt gs@GameState{ enemies = es, playerPos = p } =
--  gs { enemies = map (updateEnemy dt p) es }

-------------------------------------------------------------
-- SISTEMA DE OLEADAS (con Wave.hs moderno)
-------------------------------------------------------------

--updateWave :: Float -> GameState -> GameState
--updateWave dt gs@GameState{ wave = w, enemies = es } =
 -- let (spawned, newWave) = spawnWaveIfNeeded dt w
 -- in gs { enemies = es ++ spawned, wave = newWave }

-------------------------------------------------------------
-- UTILIDADES
-------------------------------------------------------------

dirToVector :: Direction -> (Float, Float)
dirToVector DUp        = (0, 1)
dirToVector DDown      = (0, -1)
dirToVector DLeft      = (-1, 0)
dirToVector DRight     = (1, 0)
dirToVector DUpLeft    = (-1, 1)
dirToVector DUpRight   = (1, 1)
dirToVector DDownLeft  = (-1, -1)
dirToVector DDownRight = (1, -1)

-- Convert movement vector to Direction (8 directions)
vectorToDirection :: (Float, Float) -> Direction
vectorToDirection (dx, dy)
  | dx > 0.3 && dy > 0.3   = DUpRight
  | dx < -0.3 && dy > 0.3  = DUpLeft
  | dx > 0.3 && dy < -0.3  = DDownRight
  | dx < -0.3 && dy < -0.3 = DDownLeft
  | dy > 0.3               = DUp
  | dy < -0.3              = DDown
  | dx < -0.3              = DLeft
  | dx > 0.3               = DRight
  | otherwise              = DUp  -- Default

clamp :: Float -> Float -> Float -> Float
clamp x a b = max a (min b x)
