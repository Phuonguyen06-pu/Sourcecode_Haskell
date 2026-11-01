-- Vẽ toàn bộ giao diện bằng Gloss

module UI.Render (render) where

import Graphics.Gloss hiding (bright)

import Levels.Arena       (MapTable)
import Model.Config
import Model.Describable  (Describable (..))
import Model.Types
import Model.World        (spawnP1, spawnP2)
import Model.Hp (hpValue)
import UI.Layout
import UI.Palette

render :: MapTable -> GameState -> Picture
render table gs =
  scale viewScale viewScale $
    pictures [background, content, noticePicture (notice gs)]
  where
    content = case screen gs of
      MenuScreen -> pictures $
        [ translate 0 (-50) $ heading "TANK BATTLE" ""
        , emblem (-340) 0 p1Color
        , emblem 340 0 p2Color
        ] ++ buttonPics MenuScreen

      HelpScreen -> helpPicture

      MapSelectScreen -> mapSelectPicture table gs

      PlayScreen -> playPicture gs

      PauseScreen -> pictures $
        [ playPicture gs
        , color (makeColorI 5 8 17 215) (rectangleSolid 1000 700)
        , centered 120 0.34 bright "PAUSED"
        , centered 85 0.12 muted "THE MATCH IS ON HOLD"
        ] ++ buttonPics PauseScreen

      GameOverScreen w -> pictures $
        [ heading (overTitle w) "MATCH COMPLETE"
        , color (makeColorI 255 204 55 55) (translate 0 50 (circleSolid 72))
        , color goldNeon (translate 0 50 (circle 68))
        , emblem 0 50 (winnerColor w)
        , centered (-55) 0.18 goldNeon (overBanner w)
        , centered (-88) 0.11 muted (describe (stats gs))
        , hint
        ] ++ buttonPics (GameOverScreen w)

noticePicture :: Maybe String -> Picture
noticePicture Nothing = blank
noticePicture (Just msg) = pictures
  [ infoPanel 0 255 900 48
  , centered 247 0.14 (makeColorI 255 85 100 255) msg
  ]

buttonPics :: ScreenState -> [Picture]
buttonPics s = map drawButton (buttonsFor s)

background :: Picture
background = pictures $ color navy (rectangleSolid 1000 700) :
  [ color (makeColorI 24 43 61 110) (line [(-500, y), (500, y)])
  | y <- [-350, -310 .. 350] ] ++
  [ color (makeColorI 24 43 61 110) (line [(x, -350), (x, 350)])
  | x <- [-500, -460 .. 500] ] ++
  [ color cyanNeon (line [(-490, 295), (-490, 345), (-440, 345)])
  , color pinkNeon (line [(440, -345), (490, -345), (490, -295)]) ]

heading :: String -> String -> Picture
heading s subtitle = pictures
  [ translate (-2) 0 $ centered 210 0.43 (makeColorI 0 220 255 100) s
  , translate 2 0 $ centered 210 0.43 (makeColorI 255 0 180 110) s
  , centered 210 0.42 bright s
  , centered 165 0.18 cyanNeon subtitle
  , color cyanNeon $ translate 0 140 (rectangleSolid 240 3)
  , color pinkNeon $ translate (-170) 140 (rectangleSolid 65 2)
  , color pinkNeon $ translate 170 140 (rectangleSolid 65 2)
  , color bright $ translate (-205) 140 (circleSolid 4)
  , color bright $ translate 205 140 (circleSolid 4)
  ]

centered :: Float -> Float -> Color -> String -> Picture
centered y k c s = smallText (-textWidth k s / 2) y k c s

textWidth :: Float -> String -> Float
textWidth k = (* k) . sum . map charWidth
  where
    charWidth ch
      | ch == ' ' = 52
      | ch `elem` "Il1!|:.,'" = 38
      | ch `elem` "MW@" = 104
      | ch `elem` "mw" = 94
      | otherwise = 78

smallText :: Float -> Float -> Float -> Color -> String -> Picture
smallText x y k c s = translate x y $ scale k k $ color c $ text s

infoPanel :: Float -> Float -> Float -> Float -> Picture
infoPanel x y w h = translate x y $ pictures
  [ color panel (rectangleSolid w h)
  , color (makeColorI 35 112 139 255) (rectangleWire w h)
  ]

-- Nút bấm vẽ theo bố cục trong UI.Layout. Nút thấp (< 40px) dùng chữ nhỏ hơn.
drawButton :: Button -> Picture
drawButton b = button (btnCenter b) (btnSize b) (btnLabel b)

button :: (Float, Float) -> (Float, Float) -> String -> Picture
button (cx, cy) (w, h) s =
  translate cx cy $ pictures
    [ color (makeColorI 0 240 255 35) $
        rectangleSolid (w + 12) (h + 12)

    , color panel $
        rectangleSolid w h

    , color cyanNeon $
        rectangleWire w h

    , color (makeColorI 0 240 255 110) $
        rectangleWire (w + 6) (h + 6)


    , translate (textX + 0.5) (textY - 0.5) $
        scale k k $
          color (makeColorI 0 190 220 55) (text s)


    , translate textX textY $
        pictures
          [ translate dx dy $
              scale k k $
                color (makeColorI 215 235 245 255) (text s)
          | dx <- [-0.15, 0, 0.15]
          , dy <- [-0.15, 0, 0.15]
          ]
    ]
  where

    baseK
      | s == "PAUSE (ESC)" = 0.145
      | s == "NEW RANDOM MAP" = 0.20
      | h < 40 = 0.12
      | otherwise = 0.20

    estimatedWidth = textWidth baseK s
    maxWidth = w - 28

    k = min baseK (baseK * maxWidth / max 1 estimatedWidth)


    textX = -(textWidth k s / 2) - 5
    textY = -(50 * k)

-- ---------------------------------------------------------------- Help

helpPicture :: Picture
helpPicture = pictures $
  [ heading "HOW TO PLAY" "CONTROLS & RULES"
  , infoPanel 0 (-5) 820 300
  , color (makeColorI 35 100 125 255) (line [(0, -15), (0, 120)])
  , smallText (-375) 112 0.17 p1Color "PLAYER 1  /  PINK"
  , smallText (-375) 72 0.12 bright "W / S     MOVE"
  , smallText (-375) 42 0.12 bright "A / D     TURN"
  , smallText (-375) 12 0.12 bright "SPACE     FIRE"
  , smallText 35 112 0.17 p2Color "PLAYER 2  /  BLUE"
  , smallText 35 72 0.12 bright "UP / DOWN   MOVE"
  , smallText 35 42 0.12 bright "LEFT / RIGHT TURN"
  , smallText 35 12 0.12 bright "ENTER       FIRE"
  , color cyanNeon (line [(-375, -17), (375, -17)])
  , smallText (-375) (-45) 0.12 goldNeon
      "BRICK: 1 HIT  /  STEEL: UNBREAKABLE  /  MUD: -70% SPEED"
  , smallText (-375) (-72) 0.12 bright
      "TRAP: -10 HP + STUN  /  SPRING: LAUNCH  /  TELEPORT: JUMP TO PAIR"
  , smallText (-375) (-99) 0.12 bright
      "BARREL: 5X5 BLAST, CHAIN REACTION, 40 DMG"
  , smallText (-375) (-126) 0.12 muted
      "FIRE = 2 BULLETS X 10 DMG  /  ESC: PAUSE  /  M: MENU"
  ] ++ buttonPics HelpScreen

-- ---------------------------------------------------------------- Chon map

mapSelectPicture :: MapTable -> GameState -> Picture
mapSelectPicture table gs = pictures $
  [ heading "SELECT MAP" "CHOOSE YOUR BATTLEFIELD"
  , centered 112 0.13 goldNeon "MODE: 2 PLAYERS  /  NEW RANDOM LAYOUT EACH MATCH"
  , selectedFrame (mapId gs)
  , miniMap table (-250) (-10) Map1
  , miniMap table 0 (-10) Map2
  , miniMap table 250 (-10) Map3
  , translate (-250) 0 $ centered (-85) 0.13 muted "STEEL ARENA"
  , translate 0 0 $ centered (-85) 0.13 muted "SWAMP & TELEPORT"
  , translate 250 0 $ centered (-85) 0.13 muted "BUMPER & BARREL"
  , legendItem (-340) (-115) brickColor "BRICK: 1 HIT"
  , legendItem (-70) (-115) steelEdge "STEEL: BLOCKS ALL"
  , legendItem 200 (-115) mudColor "MUD: -70% SPEED"
  , legendItem (-340) (-140) trapColor "TRAP: -10 HP + STUN"
  , legendItem (-70) (-140) springColor "SPRING: LAUNCH"
  , legendItem 200 (-140) barrelColor "BARREL: 5X5 BLAST"
  , legendItem (-340) (-165) teleColor "TELEPORT (MAP 2)"
  , hint
  ] ++ buttonPics MapSelectScreen

legendItem :: Float -> Float -> Color -> String -> Picture
legendItem x y c s = pictures
  [ translate (x + 7) (y + 6) (color c (rectangleSolid 14 14))
  , smallText (x + 20) y 0.11 bright s
  ]

selectedFrame :: MapId -> Picture
selectedFrame m = translate cx 75 $
  color goldNeon (rectangleWire 212 62)
  where
    cx = case m of
      Map1 -> -250
      Map2 -> 0
      Map3 -> 250

-- Xem trước bản đồ: phần cố định, sàn đặc biệt và các ô ngẫu nhiên (mờ).
miniMap :: MapTable -> Float -> Float -> MapId -> Picture
miniMap table x y m = translate x y $ pictures $
  [ color panel (rectangleSolid w h)
  , color (makeColorI 45 90 110 255) (rectangleWire w h)
  ]
  ++ [ dot (floorPos f) (floorColor (floorKind f)) | f <- layoutFloors lay ]
  ++ [ dot p (makeColorI 60 80 100 255) | p <- layoutRandom lay ]
  ++ [ dot (wallPos wl) (wallColor (wallType wl)) | wl <- layoutWalls lay ]
  ++ [ translate (vx spawnP1 * k) (vy spawnP1 * k) (color p1Color (circleSolid 4))
     , translate (vx spawnP2 * k) (vy spawnP2 * k) (color p2Color (circleSolid 4)) ]
  where
    lay = table m
    k   = 0.2
    w   = 2 * arenaHalfW * k
    h   = 2 * arenaHalfH * k
    dot p c = translate (vx p * k) (vy p * k) (color c (rectangleSolid (tileSize * k) (tileSize * k)))

wallColor :: WallType -> Color
wallColor SolidSteel      = steelEdge
wallColor BreakableBrick  = brickEdge
wallColor ExplosiveBarrel = barrelColor

floorColor :: FloorKind -> Color
floorColor Mud            = mudColor
floorColor Trap           = trapColor
floorColor Spring         = springColor
floorColor (Teleporter _) = teleColor

-- ---------------------------------------------------------------- Tran dau

playPicture :: GameState -> Picture
playPicture gs = pictures $
  [ arenaFrame, arenaGrid ]
  ++ map drawFloor (floors gs)
  ++ map drawWall (walls gs)
  ++ map drawExplosion (explosions gs)
  ++ map drawBullet (bullets gs)
  ++ [ drawTank p1Color (tank1 gs)
     , drawTank p2Color (tank2 gs)
     , hud gs
     ]
  ++ buttonPics PlayScreen

hud :: GameState -> Picture
hud gs = pictures
  [ infoPanel 0 310 990 70
  , smallText (-470) 315 0.15 p1Color ("P1  " ++ show (hpValue (tankHp (tank1 gs))) ++ " HP")
  , healthBar (-470) 295 p1Color (hpValue (tankHp (tank1 gs)))
  , smallText 255 315 0.15 p2Color ("P2  " ++ show (hpValue (tankHp (tank2 gs))) ++ " HP")
  , healthBar 255 295 p2Color (hpValue (tankHp (tank2 gs)))
  , centered 322 0.12 bright ("MAP " ++ show (mapNumber (mapId gs)) ++ "   TIME " ++ clock (matchTime gs))
  , infoPanel 0 (-320) 990 50
  , centered (-328) 0.11 muted (describe (stats gs))
  ]

healthBar :: Float -> Float -> Color -> Int -> Picture
healthBar x y c hp = pictures
  [ translate (x + fromIntegral i * 19) y $ color (if i < filled then hpColor else muted) (rectangleSolid 15 9)
  | i <- [0 .. 9 :: Int] ]
  where
    filled = max 0 (min 10 ((hp + 9) `div` 10))
    hpColor | hp <= 30  = makeColorI 255 65 82 255
            | hp <= 60  = goldNeon
            | otherwise = c

-- Khung và lưới ô 30px của sân 960 x 540.
arenaFrame :: Picture
arenaFrame = pictures
  [ color (makeColorI 0 220 245 110) (rectangleWire 966 546)
  , color cyanNeon (line [(-484, 234), (-484, 274), (-444, 274)])
  , color cyanNeon (line [(484, 234), (484, 274), (444, 274)])
  , color pinkNeon (line [(-484, -234), (-484, -274), (-444, -274)])
  , color pinkNeon (line [(484, -234), (484, -274), (444, -274)])
  ]

arenaGrid :: Picture
arenaGrid = color (makeColorI 30 41 59 255) $ pictures $
  [ line [(x, -arenaHalfH), (x, arenaHalfH)]
  | x <- [-arenaHalfW, -arenaHalfW + tileSize .. arenaHalfW] ] ++
  [ line [(-arenaHalfW, y), (arenaHalfW, y)]
  | y <- [-arenaHalfH, -arenaHalfH + tileSize .. arenaHalfH] ]

ownerColor :: PlayerId -> Color
ownerColor P1 = p1Color
ownerColor P2 = p2Color

-- Xe tăng (thu nhỏ cho vừa ô 30px). Quầng tím = đang choáng, quầng vàng = đang bị lò xo bắn.
drawTank :: Color -> Tank -> Picture
drawTank c t =
  translate (vx p) (vy p) $ pictures
    [ rotate (negate (tankAngle t)) (scale 0.62 0.62 (tankShape c))
    , statusRing (tankStatus t)
    ]
  where p = tankPos t

statusRing :: Status -> Picture
statusRing st
  | stunFrames st > 0   = color trapColor (thickCircle 16 2)
  | launchFrames st > 0 = color springColor (thickCircle 16 2)
  | otherwise           = blank

drawBullet :: Bullet -> Picture
drawBullet b = translate (vx p) (vy p) $ pictures
  [ color (withAlpha 0.3 c) (circleSolid (bulletRadius * 2.2))
  , color c (circleSolid (bulletRadius * 1.2))
  , color bright (circleSolid (bulletRadius * 0.5))
  ]
  where p = bulletPos b
        c = ownerColor (bulletOwner b)

drawWall :: Wall -> Picture
drawWall w = translate (vx p) (vy p) $ case wallType w of
  BreakableBrick -> pictures
    [ color brickColor (rectangleSolid (s - 2) (s - 2))
    , color brickEdge (rectangleWire (s - 6) (s - 6))
    ]
  SolidSteel -> pictures
    [ color steelFill (rectangleSolid (s - 2) (s - 2))
    , color steelEdge (rectangleWire (s - 4) (s - 4))
    , color steelEdge (rectangleWire (s - 5) (s - 5))
    ]
  ExplosiveBarrel -> pictures
    [ color barrelColor (rectangleSolid (s - 8) (s - 8))
    , color barrelCore (rectangleSolid (s - 20) (s - 20))
    ]
  where p = wallPos w
        s = wallHalfSize * 2

drawFloor :: Floor -> Picture
drawFloor f = translate (vx p) (vy p) $ case floorKind f of
  Mud -> pictures
    [ color mudColor (rectangleSolid s s)
    , color mudEdge (rectangleWire s s)
    ]
  Trap -> pictures
    [ color trapColor (rectangleSolid (s - 6) (s - 6))
    , color trapEdge (rectangleWire (s - 2) (s - 2))
    ]
  Spring -> pictures
    [ color springColor (rectangleSolid (s - 4) (s - 4))
    , color (makeColorI 120 80 0 255) (line [(-6, -4), (0, 5), (6, -4)])
    ]
  Teleporter _ -> pictures
    [ color teleColor (thickCircle 11 2)
    , color teleColor (circle 5)
    ]
  where p = floorPos f
        s = tileSize

drawExplosion :: Explosion -> Picture
drawExplosion e = translate (vx p) (vy p) $ pictures
  [ color (makeColorI 239 68 68 128) (circleSolid r)
  , color barrelCore (thickCircle (max 0 (r - 1.5)) 3)
  ]
  where p = expPos e
        r = expRadius e

-- xe tăng trên menu
emblem :: Float -> Float -> Color -> Picture
emblem x y c = translate x y $ pictures
  [ color (makeColorI 12 35 50 255) (circleSolid 49)
  , color c (circle 49)
  , scale 1.25 1.25 (tankShape c)
  ]

-- hình dạng xe tăng
tankShape :: Color -> Picture
tankShape c = pictures
  [ color (makeColorI 0 180 220 35) (rectangleSolid 45 42)
  , color (makeColorI 16 25 38 255) (translate 0 18 (rectangleSolid 42 10))
  , color (makeColorI 16 25 38 255) (translate 0 (-18) (rectangleSolid 42 10))
  , color c (translate 0 18 (rectangleWire 42 10))
  , color c (translate 0 (-18) (rectangleWire 42 10))
  , color (makeColorI 95 125 145 255) $ pictures
      [ translate x y (rectangleSolid 2.5 7)
      | x <- [-16, -8, 0, 8, 16], y <- [-18, 18] ]
  , color (makeColorI 22 48 64 255) (polygon hull)
  , color c (lineLoop hull)
  , color c (polygon [(-13, -9), (10, -9), (15, 0), (10, 9), (-13, 9), (-17, 0)])
  , color (makeColorI 14 30 44 255) (circleSolid 11)
  -- Hai nong sung
  , color bright (translate 20 5 (rectangleSolid 24 4))
  , color bright (translate 20 (-5) (rectangleSolid 24 4))
  , color c (translate 33 5 (rectangleSolid 4 6))
  , color c (translate 33 (-5) (rectangleSolid 4 6))
  -- Thap phao
  , color c (circleSolid 10)
  , color (makeColorI 14 30 44 255) (circleSolid 7)
  , color bright (circle 5)
  , color bright (circleSolid 2.5)
  ]
  where hull = [(-18, -10), (-10, -14), (13, -14), (19, -8), (19, 8), (13, 14), (-10, 14), (-18, 10)]

hint :: Picture
hint = smallText (-480) (-334) 0.12 muted "M: MENU   ESC: PAUSE"

overTitle :: Winner -> String
overTitle Draw        = "DRAW!"
overTitle (Winner P1) = "PLAYER 1 WINS!"
overTitle (Winner P2) = "PLAYER 2 WINS!"

overBanner :: Winner -> String
overBanner Draw = "MATCH DRAW"
overBanner _    = "VICTORY"

winnerColor :: Winner -> Color
winnerColor (Winner P2) = p2Color
winnerColor (Winner P1) = p1Color
winnerColor Draw        = goldNeon

-- Dong ho m:ss
clock :: Int -> String
clock t = show (t `div` 60) ++ ":" ++ pad (t `mod` 60)
  where pad n = if n < 10 then '0' : show n else show n
