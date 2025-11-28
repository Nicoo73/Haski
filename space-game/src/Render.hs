module Render
  ( render
  ) where

import Graphics.Gloss
import GameState
import Assets
import Enemy
import Item
import Boss 
import Input (buttonX, buttonY)


-- Dimensiones de la imagen de fondo del menú
-- Ajustado según tu captura de pantalla (1024x572)
menuBgW, menuBgH :: Float
menuBgW = 1024.0 
menuBgH = 572.0 

-------------------------------------------------------------
-- RENDER GENERAL
-------------------------------------------------------------

render :: Assets -> GameState -> Picture
render assets gs = case currentScreen gs of
    Menu    -> drawMenuScreen assets gs
    Playing -> drawGameScreen assets gs
    GameOver -> drawGameOverScreen assets gs

drawGameScreen :: Assets -> GameState -> Picture
drawGameScreen assets gs =
    pictures [ scaledBackground
             , drawItems assets gs 
             , drawEnemies assets gs
             , drawBoss assets gs
             , drawBossAttacks assets gs
             , drawBullets gs
             , drawEnemyBullets gs
             , scaledPlayer
             , drawHealthBar gs
             , drawBossHealthBar gs
             , drawHUD assets gs  
             ]
  where
    (winW, winH) = windowSize gs

    terrainW = 480.0
    terrainH = 360.0
    
    scaleX = fromIntegral winW / terrainW
    scaleY = fromIntegral winH / terrainH

    background = aBackground assets
    scaledBackground = scale scaleX scaleY background

    rotAngle = case playerDir gs of
      DUp        -> 0
      DDown      -> 180
      DLeft      -> 270
      DRight     -> 90
      DUpLeft    -> 315 
      DUpRight   -> 45  
      DDownLeft  -> 225 
      DDownRight -> 135 

    (px, py) = playerPos gs

    frames = aPlayerFrames assets
    numFrames = max 1 (length frames)
    frameIndex = floor (animTime gs * 8.0) `mod` numFrames
    playerSprite = frames !! frameIndex

    scaledPlayer = translate (px * scaleX) (py * scaleY) $ scale scaleX scaleY $ rotate rotAngle playerSprite

    
-------------------------------------------------------------
-- RENDER DE MENÚ
-------------------------------------------------------------

drawMenuScreen :: Assets -> GameState -> Picture
drawMenuScreen assets gs =
    pictures [ scaledMenuBackground
             , drawPlayButton
             ]
  where
    (winWInt, winHInt) = windowSize gs
    winW = fromIntegral winWInt
    winH = fromIntegral winHInt
    
    menuScaleW = winW / menuBgW
    menuScaleH = winH / menuBgH
    menuScaleF = max menuScaleW menuScaleH 

    scaledMenuBackground = scale menuScaleF menuScaleF (aMenuBackground assets)

    -- NUEVA LÓGICA PARA EL BOTÓN:
    btnScaleFactor = 0.4 -- Factor de escala (40% del tamaño original)
    playButtonPic = aPlayButton assets -- Obtenemos la imagen
    
    -- Dibujamos: primero escalamos, luego movemos a la posición de Input.hs
    drawPlayButton = translate buttonX buttonY $ scale btnScaleFactor btnScaleFactor playButtonPic


-------------------------------------------------------------
-- NUEVO: RENDER PANTALLA DERROTA
-------------------------------------------------------------
drawGameOverScreen :: Assets -> GameState -> Picture
drawGameOverScreen assets gs =
    pictures [ scaledBg, drawButton ]
  where
    (winWInt, winHInt) = windowSize gs
    winW = fromIntegral winWInt
    winH = fromIntegral winHInt

    -- 1. Fondo (derrota.png - 2048x2048)
    -- Lo escalamos para cubrir toda la pantalla
    bg = aGameOverBackground assets
    bgW = 1024.0
    bgH = 572.0
    scaleW = winW / bgW
    scaleH = winH / bgH
    scaleBg = max scaleW scaleH -- "Cover" fit
    
    scaledBg = scale scaleBg scaleBg bg

    -- 2. Botón (botonderrota.png - 3584x1184)
    -- Lo escalamos y posicionamos abajo
    btnPic = aGameOverButton assets
    
    -- Definimos tamaño deseado en pantalla
    targetBtnW = 250.0 -- Ancho visual del botón
    originalBtnW = 632.0
    scaleBtn = targetBtnW / originalBtnW

    -- Coordenadas (deben coincidir con Input.hs)
    btnX = 0.0
    btnY = -100.0 

    drawButton = translate btnX btnY $ scale scaleBtn scaleBtn btnPic

-------------------------------------------------------------
-- RENDER DE BARRA DE VIDA
-------------------------------------------------------------

drawHealthBar :: GameState -> Picture
drawHealthBar gs = 
    translate barX barY $ 
    pictures [ bgBar, fillBar, borderBar ]
  where
    (winWInt, winHInt) = windowSize gs
    
    maxHP = 100.0
    -- CORRECCIÓN: Usamos currentHealth en lugar de playerHealth
    currentHP = fromIntegral (currentHealth gs)
    barW = 100.0
    barH = 10.0
    
    margin = 20.0
    barX = -fromIntegral winWInt / 2 + margin + (barW / 2)
    barY = fromIntegral winHInt / 2 - margin - (barH / 2)

    fillPct = max 0 (currentHP / maxHP)
    fillW = barW * fillPct

    bgBar = color red $ rectangleSolid barW barH
    fillBar = translate (-(barW - fillW)/2) 0 $
              color green $ rectangleSolid fillW barH
    borderBar = color white $ rectangleWire barW barH

-------------------------------------------------------------
-- RENDER DE PROYECTILES
-------------------------------------------------------------

drawBullets :: GameState -> Picture
drawBullets gs = pictures (map drawBullet (bullets gs))
  where
    (winW, winH) = windowSize gs
    terrainW = 480.0
    terrainH = 360.0
    
    scaleX = fromIntegral winW / terrainW
    scaleY = fromIntegral winH / terrainH

    drawBullet bullet =
        let (x, y) = bulletPos bullet
        in translate (x * scaleX) (y * scaleY) $ scale scaleX scaleY $
             color yellow (circleSolid 3)

drawEnemyBullets :: GameState -> Picture
drawEnemyBullets gs = pictures (map drawEnemyBullet (enemyBullets gs))
  where
    (winW, winH) = windowSize gs
    terrainW = 480.0
    terrainH = 360.0
    scaleX = fromIntegral winW / terrainW
    scaleY = fromIntegral winH / terrainH

    drawEnemyBullet eBullet =
        let (x, y) = eBulletPos eBullet
        in translate (x * scaleX) (y * scaleY) $ scale scaleX scaleY $
             color red (circleSolid 3) 

-------------------------------------------------------------
-- RENDER DE ENEMIGOS
-------------------------------------------------------------

drawEnemies :: Assets -> GameState -> Picture
drawEnemies assets gs = pictures (map drawEnemy (enemies gs))
  where
    (winW, winH) = windowSize gs
    terrainW = 480.0
    terrainH = 360.0
    scaleX = fromIntegral winW / terrainW
    scaleY = fromIntegral winH / terrainH

    drawEnemy enemy =
        let (x, y) = enemyPos enemy
            enemySprite = case enemyType enemy of
              Alien1 -> aAlien1Sprite assets
              Alien2 -> aAlien2Sprite assets
              Alien3 -> aAlien3Sprite assets
        in translate (x * scaleX) (y * scaleY) $ scale scaleX scaleY $ enemySprite

-------------------------------------------------------------
-- RENDER DE ITEMS
-------------------------------------------------------------

drawItems :: Assets -> GameState -> Picture
drawItems assets gs = pictures (map drawItem (items gs))
  where
    (winW, winH) = windowSize gs
    terrainW = 480.0
    terrainH = 360.0
    scaleX = fromIntegral winW / terrainW
    scaleY = fromIntegral winH / terrainH

    drawItem item =
      let (x,y) = itemPos item
          itemSprite = case itemType item of
              HealSmall   -> aHealSmallSprite assets
              SpeedBoost  -> aSpeedBoostSprite assets
              DamageBoost -> aDamageBoostSprite assets
      in translate (x * scaleX) (y * scaleY) $ scale scaleX scaleY $ itemSprite

-------------------------------------------------------------
-- RENDER DEL BOSS
-------------------------------------------------------------

drawBoss :: Assets -> GameState -> Picture
drawBoss assets gs = case maybeBoss gs of
  Nothing -> blank
  Just boss -> 
    let (winW, winH) = windowSize gs
        terrainW = 480.0
        terrainH = 360.0
        scaleX = fromIntegral winW / terrainW
        scaleY = fromIntegral winH / terrainH
        (x, y) = bossPos boss
    in translate (x * scaleX) (y * scaleY) $ scale scaleX scaleY $ aBossSprite assets

drawBossAttacks :: Assets -> GameState -> Picture
drawBossAttacks assets gs = pictures (map drawAttack (bossAttacks gs))
  where
    (winW, winH) = windowSize gs
    terrainW = 480.0
    terrainH = 360.0
    scaleX = fromIntegral winW / terrainW
    scaleY = fromIntegral winH / terrainH

    drawAttack attack =
      let (x, y) = attackPos attack
          attackSprite = case attackType attack of
            AT1Arrows -> aAttack1Sprite assets
            AT2Ball -> aAttack2Sprite assets
          
          -- Si es AT1 y va hacia la izquierda, voltear el sprite horizontalmente
          shouldFlip = attackType attack == AT1Arrows && attackDir attack == DLeft
          flippedSprite = if shouldFlip
                         then scale (-1) 1 attackSprite  -- Voltear horizontalmente
                         else attackSprite
          
      in translate (x * scaleX) (y * scaleY) $ scale scaleX scaleY $ flippedSprite

drawBossHealthBar :: GameState -> Picture
drawBossHealthBar gs = case maybeBoss gs of
  Nothing -> blank
  Just boss ->
    let (winWInt, winHInt) = windowSize gs
        maxHP = fromIntegral (bossMaxHealth boss)
        currentHP = fromIntegral (bossHealth boss)
        barW = 200.0
        barH = 15.0
        
        margin = 20.0
        barX = 0  -- Centrado horizontalmente
        barY = fromIntegral winHInt / 2 - margin - (barH / 2) - 30  -- Debajo de la barra del jugador
        
        fillPct = max 0 (currentHP / maxHP)
        fillW = barW * fillPct
        
        bgBar = color (makeColorI 50 50 50 255) $ rectangleSolid barW barH
        fillBar = translate (-(barW - fillW)/2) 0 $
                  color (makeColorI 150 0 150 255) $ rectangleSolid fillW barH
        borderBar = color white $ rectangleWire barW barH
        
        label = translate (-60) (barH + 5) $ 
                scale 0.15 0.15 $ color white $ text "BOSS"
        
    in translate barX barY $ pictures [bgBar, fillBar, borderBar, label]

-------------------------------------------------------------
-- RENDER DEL HUD (Opcional)
-------------------------------------------------------------

drawHUD :: Assets -> GameState -> Picture
drawHUD assets gs = pictures [damagePic, damageValue, speedPic, speedValue]
  where
    (winWInt, winHInt) = windowSize gs
    stats = currentStats gs

    dmgBase  = playerDamage stats
    dmgBonus = playerDamageBonus stats
    spdBase  = playerMoveSpeed stats
    spdBonus = playerSpeedBonus stats

    -- Imagen del rótulo "Daño"
    damagePic = translate (-fromIntegral winWInt / 2 + 50)
                          (fromIntegral winHInt / 2 - 50) $
                scale 0.5 0.5 (aDamageText assets)

    -- Valor numérico al lado de la imagen
    damageValue = translate (-fromIntegral winWInt / 2 + 95)
                             (fromIntegral winHInt / 2 - 55) $
                  scale 0.15 0.15 $
                  color white $
                  text (": " ++ show dmgBase ++ " (+" ++ show dmgBonus ++ ")")

    -- Imagen del rótulo "Velocidad"
    speedPic = translate (-fromIntegral winWInt / 2 + 80)
                          (fromIntegral winHInt / 2 - 85) $
                scale 0.5 0.5 (aSpeedText assets)

    -- Velocidad (texto normal)
    speedValue  = translate (-fromIntegral winWInt / 2 + 170)
                           (fromIntegral winHInt / 2 - 90) $
                 scale 0.15 0.15 $
                 color white $
                 text (": " ++ show spdBase ++ " (+" ++ show spdBonus ++ ")")
