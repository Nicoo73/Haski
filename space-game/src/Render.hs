module Render
  ( render
  ) where

import Graphics.Gloss
import GameState
import Assets
import Enemy
import Item
import Boss 
import Input (buttonX, buttonY, controlsBtnX, controlsBtnY)

-- Dimensiones de la imagen de fondo del menú
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
    Controls -> drawControlsScreen assets gs

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
             , drawWaveCounter gs
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
    frameIndex = if numFrames == 0 then 0 else floor (animTime gs * 8.0) `mod` numFrames
    
    playerSprite = if null frames 
                   then color blue (rectangleSolid 16 16)
                   else frames !! frameIndex

    scaledPlayer = translate (px * scaleX) (py * scaleY) $ scale scaleX scaleY $ rotate rotAngle playerSprite

    
-------------------------------------------------------------
-- RENDER DE MENÚ
-------------------------------------------------------------

drawMenuScreen :: Assets -> GameState -> Picture
drawMenuScreen assets gs =
    pictures [ scaledMenuBackground
             , drawPlayButton
             , drawControlsButton 
             ]
  where
    (winWInt, winHInt) = windowSize gs
    winW = fromIntegral winWInt
    winH = fromIntegral winHInt
    
    menuScaleW = winW / menuBgW
    menuScaleH = winH / menuBgH
    menuScaleF = max menuScaleW menuScaleH 

    scaledMenuBackground = scale menuScaleF menuScaleF (aMenuBackground assets)

    -- Botón Jugar
    btnScaleFactor = 0.4 
    playButtonPic = aPlayButton assets 
    drawPlayButton = translate buttonX buttonY $ scale btnScaleFactor btnScaleFactor playButtonPic
    
    -- Botón Controles
    controlsPic = aControlsButton assets
    drawControlsButton = translate controlsBtnX controlsBtnY $ scale btnScaleFactor btnScaleFactor controlsPic


-------------------------------------------------------------
-- RENDER PANTALLA CONTROLES (Con Fondo)
-------------------------------------------------------------

drawControlsScreen :: Assets -> GameState -> Picture
drawControlsScreen assets gs =
    pictures [ scaledBg ]
  where
    (winWInt, winHInt) = windowSize gs
    winW = fromIntegral winWInt
    winH = fromIntegral winHInt
    
    -- Dimensiones de fondocontroles.jpg
    bgW = 1024.0
    bgH = 572.0

    -- Calcular escala para cubrir la pantalla (estilo "cover")
    scaleW = winW / bgW
    scaleH = winH / bgH
    finalScale = max scaleW scaleH 

    bgPic = aControlsBackground assets
    scaledBg = scale finalScale finalScale bgPic


-------------------------------------------------------------
-- RENDER PANTALLA DERROTA
-------------------------------------------------------------
drawGameOverScreen :: Assets -> GameState -> Picture
drawGameOverScreen assets gs =
    pictures [ scaledBg, drawButton ]
  where
    (winWInt, winHInt) = windowSize gs
    winW = fromIntegral winWInt
    winH = fromIntegral winHInt

    bg = aGameOverBackground assets
    bgW = 1024.0
    bgH = 572.0
    scaleW = winW / bgW
    scaleH = winH / bgH
    scaleBg = max scaleW scaleH 
    
    scaledBg = scale scaleBg scaleBg bg

    btnPic = aGameOverButton assets
    targetBtnW = 250.0 
    originalBtnW = 632.0
    scaleBtn = targetBtnW / originalBtnW

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
          
          shouldFlip = attackType attack == AT1Arrows && attackDir attack == DLeft
          flippedSprite = if shouldFlip
                          then scale (-1) 1 attackSprite  
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
        barX = 0  
        barY = fromIntegral winHInt / 2 - margin - (barH / 2) - 30 
        
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
-- RENDER DEL HUD
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

    damagePic = translate (-fromIntegral winWInt / 2 + 50)
                          (fromIntegral winHInt / 2 - 50) $
                scale 0.5 0.5 (aDamageText assets)

    damageValue = translate (-fromIntegral winWInt / 2 + 95)
                             (fromIntegral winHInt / 2 - 55) $
                  scale 0.15 0.15 $
                  color white $
                  text (": " ++ show dmgBase ++ " (+" ++ show dmgBonus ++ ")")

    speedPic = translate (-fromIntegral winWInt / 2 + 80)
                          (fromIntegral winHInt / 2 - 85) $
                scale 0.25 0.25 (aSpeedText assets)

    speedValue  = translate (-fromIntegral winWInt / 2 + 170)
                           (fromIntegral winHInt / 2 - 90) $
                 scale 0.15 0.15 $
                 color white $
                 text (": " ++ show spdBase ++ " (+" ++ show spdBonus ++ ")")

-------------------------------------------------------------
-- CONTADOR DE OLEADAS
-------------------------------------------------------------

drawWaveCounter :: GameState -> Picture
drawWaveCounter gs =
  -- Solo mostrar si no ha aparecido el boss y estamos en oleadas 1, 2 o 3
  if waveCount gs <= 3 && not (bossSpawned gs)
  then
    let (winWInt, winHInt) = windowSize gs
        -- Calcular completado basado en waveCount y enemiesLeft
        completedWaves = case waveCount gs of
                          1 -> 0  -- Wave 1 activa, 0 completadas
                          2 -> if enemiesLeft (wave gs) > 0 then 1 else 2  -- Wave 2 activa
                          3 -> if enemiesLeft (wave gs) > 0 then 2 else 3  -- Wave 3 activa
                          _ -> 0
        waveText = "Wave " ++ show completedWaves ++ "/3"
        
        -- Posición: arriba a la derecha
        xPos = fromIntegral winWInt / 2 - 120
        yPos = fromIntegral winHInt / 2 - 50
        
    in translate xPos yPos $ scale 0.2 0.2 $ color white $ text waveText
  else blank
