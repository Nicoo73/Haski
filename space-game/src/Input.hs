module Input
  ( handleEvent
  , updateGameState
  , dirToVector
  , buttonX
  , buttonY
  , controlsBtnX
  , controlsBtnY
  ) where

import Graphics.Gloss.Interface.Pure.Game
import GameState
import Enemy (vectorToDirection)
import qualified Enemy as E
import Wave

-------------------------------------------------------------
-- CONSTANTES DE BOTONES
-------------------------------------------------------------

-- Botón Jugar
buttonX, buttonY :: Float
buttonX = 0.0
buttonY = -150.0

buttonW, buttonH :: Float
buttonW = 815.0 * 0.4  
buttonH = 205.0 * 0.4  

-- Botón Controles
controlsBtnX, controlsBtnY :: Float
controlsBtnX = 0.0
controlsBtnY = -270.0  

controlsBtnW, controlsBtnH :: Float
controlsBtnW = 966.0 * 0.4  
controlsBtnH = 230.0 * 0.4  

-- Botón Game Over y Victoria
goButtonX, goButtonY, goButtonW, goButtonH :: Float
goButtonX = 0.0
goButtonY = -100.0 
goButtonW = 200.0
goButtonH = 60.0

-------------------------------------------------------------
-- EVENTOS
-------------------------------------------------------------

handleEvent :: Event -> GameState -> GameState

-- 1. Clic en el Menú
handleEvent (EventKey (MouseButton LeftButton) Down _ (mx, my)) gs 
  | currentScreen gs == Menu = handleMenuClick mx my gs

-- 2. Clic en pantalla de Derrota
handleEvent (EventKey (MouseButton LeftButton) Down _ (mx, my)) gs 
  | currentScreen gs == GameOver = handleGameOverClick mx my gs

-- 3. NUEVO: Clic en pantalla de Victoria (funciona igual que Game Over)
handleEvent (EventKey (MouseButton LeftButton) Down _ (mx, my)) gs 
  | currentScreen gs == Victory = handleGameOverClick mx my gs

-- 4. SALIR DE CONTROLES
handleEvent (EventKey (MouseButton LeftButton) Down _ _) gs
  | currentScreen gs == Controls = gs { currentScreen = Menu }

handleEvent (EventKey (SpecialKey KeyEsc) Down _ _) gs
  | currentScreen gs == Controls = gs { currentScreen = Menu }


-- 5. Teclas de Juego (Solo funcionan si estamos en Playing)
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
    (' ', Down) -> shootBullet gs
    _           -> gs

-- 6. Disparo con Espacio
handleEvent (EventKey (SpecialKey KeySpace) Down _ _) gs 
  | currentScreen gs == Playing = shootBullet gs

-- 7. Resize de ventana
handleEvent (EventResize newSize) gs =
  gs { windowSize = newSize }

handleEvent _ gs = gs

-------------------------------------------------------------
-- LÓGICA DE MENÚ
-------------------------------------------------------------

handleMenuClick :: Float -> Float -> GameState -> GameState
handleMenuClick mx my gs
  | isInsidePlay mx my = gs { currentScreen = Playing }
  | isInsideControls mx my = gs { currentScreen = Controls }
  | otherwise = gs
  where
    isInsidePlay x y =
      x >= buttonX - buttonW / 2 &&
      x <= buttonX + buttonW / 2 &&
      y >= buttonY - buttonH / 2 &&
      y <= buttonY + buttonH / 2
      
    isInsideControls x y =
      x >= controlsBtnX - controlsBtnW / 2 &&
      x <= controlsBtnX + controlsBtnW / 2 &&
      y >= controlsBtnY - controlsBtnH / 2 &&
      y <= controlsBtnY + controlsBtnH / 2

-------------------------------------------------------------
-- LÓGICA DE GAME OVER / VICTORIA
-------------------------------------------------------------

handleGameOverClick :: Float -> Float -> GameState -> GameState
handleGameOverClick mx my gs
  | isInsideButton mx my = initialState { windowSize = windowSize gs } 
  | otherwise = gs
  where
    isInsideButton x y =
      x >= goButtonX - goButtonW / 2 &&
      x <= goButtonX + goButtonW / 2 &&
      y >= goButtonY - goButtonH / 2 &&
      y <= goButtonY + goButtonH / 2
       
-------------------------------------------------------------
-- FUNCIONES DE JUEGO
-------------------------------------------------------------

shootBullet :: GameState -> GameState
shootBullet gs@GameState{ playerPos = pPos, playerDir = pDir, bullets = bs, currentStats = cs } =
  let 
    totalDamage = playerDamage cs + playerDamageBonus cs 
    newBullet = Bullet { 
        bulletPos   = pPos, 
        bulletDir   = pDir, 
        bulletSpeed = playerBulletSpeed cs, 
        bulletDamage = totalDamage           
    }
  in gs { bullets = newBullet : bs }

-------------------------------------------------------------
-- UPDATE GAME STATE
-------------------------------------------------------------

updateGameState :: Float -> GameState -> GameState
updateGameState dt =
      updatePlayer dt        
  .   updateBullets dt       
  .   updateEnemyBullets dt 

-- Movimiento del jugador
updatePlayer :: Float -> GameState -> GameState
updatePlayer dt gs@GameState{ currentStats = cs } = gs
  { playerPos = newPos
  , playerDir = newDir
  , animTime = animTime gs + dt
  }
  where
    keys = keysDown gs
    (dx, dy) = foldr addVector (0, 0) (map dirToVector keys)
    
    newDir = if dx == 0 && dy == 0
             then playerDir gs 
             else vectorToDirection (dx, dy)

    magnitude = sqrt (dx*dx + dy*dy)
    (ndx, ndy) = if magnitude > 0 then (dx/magnitude, dy/magnitude) else (0,0)

    totalMoveSpeed = playerMoveSpeed cs + playerSpeedBonus cs
    
    moveX = ndx * totalMoveSpeed * dt
    moveY = ndy * totalMoveSpeed * dt

    (px, py) = playerPos gs

    terrainW = 640.0  
    terrainH = 360.0
    margin = 16.0  
    minX = -terrainW / 2 + margin
    maxX = terrainW / 2 - margin
    minY = -terrainH / 2 + margin
    maxY = terrainH / 2 - margin

    newX = clamp (px + moveX) minX maxX
    newY = clamp (py + moveY) minY maxY
    newPos = (newX, newY)

    addVector (a,b) (c,d) = (a+c, b+d)

-- Movimiento de balas del jugador
updateBullets :: Float -> GameState -> GameState
updateBullets dt gs@GameState{ bullets = bs } =
  gs { bullets = filter inBounds (map (moveBullet dt) bs) }
  where
    moveBullet deltaTime bullet =
      let (bx, by) = bulletPos bullet
          dir = bulletDir bullet
          speed = bulletSpeed bullet
          (dx, dy) = dirToVector dir
          newX = bx + dx * speed * deltaTime
          newY = by + dy * speed * deltaTime
      in bullet { bulletPos = (newX, newY) }
    
    inBounds bullet =
      let (bx, by) = bulletPos bullet
          terrainW = 640.0
          terrainH = 360.0
          margin = 50.0  
      in abs bx < (terrainW / 2 + margin) && abs by < (terrainH / 2 + margin)

-- Movimiento de balas enemigas
updateEnemyBullets :: Float -> GameState -> GameState
updateEnemyBullets dt gs@GameState{ enemyBullets = ebs } =
  gs { enemyBullets = filter inBounds (map (moveEnemyBullet dt) ebs) }
  where
    moveEnemyBullet deltaTime eBullet =
      let (bx, by) = eBulletPos eBullet
          dir = eBulletDir eBullet
          speed = eBulletSpeed eBullet
          (dx, dy) = dirToVector dir
          newX = bx + dx * speed * deltaTime
          newY = by + dy * speed * deltaTime
      in eBullet { eBulletPos = (newX, newY) }
    
    inBounds eBullet =
      let (bx, by) = eBulletPos eBullet
          terrainW = 640.0
          terrainH = 360.0
          margin = 50.0
      in abs bx < (terrainW / 2 + margin) && abs by < (terrainH / 2 + margin)

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

clamp :: Float -> Float -> Float -> Float
clamp x a b = max a (min b x)