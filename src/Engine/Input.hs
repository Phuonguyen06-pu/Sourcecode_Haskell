-- Nhận phím (2 người cùng bàn phím) và click chuột
-- Giữ phím => activeKeys (không bị khựng khi nhiều phím cùng lúc)
-- Bắn: tạo đạn đúng lúc nhấn phím, có hồi chiêu ('tryFire')
-- ESC: tạm dừng / tiếp tục.  M: về menu chính
-- Vị trí nút lấy từ UI.Layout (dùng chung với UI.Render)

module Engine.Input
  ( handleEvent
  , spawnBullet
  , tryFire
  , startMatch
  ) where

import Data.List (nub)
import UI.Events (UiEvent(..))

import Engine.Math  (degToRad, vecAdd)
import Levels.Arena (MapTable, buildArena)
import Model.Config
import Model.Types
import Model.World  (cooldownOf, initGameState, setCooldown, tankOf)
import UI.Layout    (ButtonId (..), buttonAt)

-- xử lý các sự kiện Gloss (chuột, bàn phím) thành hành động game
handleEvent :: MapTable -> UiEvent -> GameState -> GameState
handleEvent table ev gs =
  handleEventCore table ev (clearNotice ev gs)

-- xóa thông báo khi click chuột, ESC, M, hoặc nhấn phím
clearNotice :: UiEvent -> GameState -> GameState
clearNotice ev gs =
  case ev of
    UiClick _  -> gs { notice = Nothing }
    UiPause    -> gs { notice = Nothing }
    UiMenu     -> gs { notice = Nothing }
    UiPress _  -> gs { notice = Nothing }
    _          -> gs

-- điều khiển game: click chuột, nhấn phím, tạm dừng, về menu
handleEventCore :: MapTable -> UiEvent -> GameState -> GameState
handleEventCore table ev gs =
  case ev of
    UiClick pos -> handleClick table pos gs
    UiPause     -> togglePause gs
    UiMenu      -> toMenu gs
    UiPress c   -> press c gs
    UiRelease c -> release c gs
    UiIgnore    -> gs

press :: Control -> GameState -> GameState
press c gs
  | playing && c == P1Fire = tryFire P1 gs'
  | playing && c == P2Fire = tryFire P2 gs'
  | otherwise = gs'
  where
    playing = screen gs == PlayScreen
    gs' = gs { activeKeys = nub (c : activeKeys gs) }

release :: Control -> GameState -> GameState
release c gs = gs { activeKeys = filter (/= c) (activeKeys gs) }

-- ESC: đang chơi thì tạm dừng, đang tạm dừng thì chơi tiếp
togglePause :: GameState -> GameState
togglePause gs = case screen gs of
  PlayScreen  -> gs { screen = PauseScreen }
  PauseScreen -> gs { screen = PlayScreen }
  _           -> gs

-- về menu chính, giữ lại map đang chọn và hạt giống ngẫu nhiên
toMenu :: GameState -> GameState
toMenu gs = (initGameState [])
  { rngSeed = rngSeed gs, mapId = mapId gs, activeKeys = activeKeys gs }

--  Bắt đầu một trận mới trên map: dựng sân và đặt lại xe, máu, đạn
startMatch :: MapTable -> MapId -> GameState -> GameState
startMatch table m gs = (initGameState ws)
  { screen     = PlayScreen
  , mapId      = m
  , floors     = fs
  , rngSeed    = seed'
  , activeKeys = activeKeys gs
  }
  where
    (ws, fs, seed') = buildArena (rngSeed gs) (table m)

-- Bắn: tạo đạn đúng lúc nhấn phím, có hồi chiêu ('tryFire')
tryFire :: PlayerId -> GameState -> GameState
tryFire p gs
  | cooldownOf p gs > 0 = gs
  | otherwise           = setCooldown p fireCooldown (spawnBullet p gs)

-- tạo đạn mới, đặt vào danh sách đạn và cộng thống kê bắn
spawnBullet :: PlayerId -> GameState -> GameState
spawnBullet owner gs = gs { bullets = pair ++ bullets gs
                          , stats   = stats gs <> shotStats owner }
  where
    t    = tankOf owner gs
    a    = degToRad (tankAngle t)
    dir  = Vec2 (cos a) (sin a)
    side = Vec2 (negate (sin a)) (cos a)       -- vuông góc với hướng nòng
    muzzle k = vecAdd (tankPos t)
                 (Vec2 (vx dir * muzzleDistance + vx side * k)
                       (vy dir * muzzleDistance + vy side * k))
    vel    = Vec2 (vx dir * bulletSpeed) (vy dir * bulletSpeed)
    shot k = Bullet (muzzle k) vel owner True
    pair   = [shot barrelOffset, shot (negate barrelOffset)]

-- Xử lý sự kiện click chuột
handleClick :: MapTable -> (Float, Float) -> GameState -> GameState
handleClick table pos gs = maybe gs (\b -> applyButton table b gs) (buttonAt (screen gs) pos)

applyButton :: MapTable -> ButtonId -> GameState -> GameState
applyButton table b gs = case b of
  BtnPlay       -> gs { screen = MapSelectScreen }
  BtnHelp       -> gs { screen = HelpScreen }
  BtnBack       -> gs { screen = if screen gs == MapSelectScreen || screen gs == HelpScreen
                                   then MenuScreen else screen gs }
  BtnMap m -> gs { mapId = m }
  BtnStart      -> startMatch table (mapId gs) gs
  BtnRematch    -> startMatch table (mapId gs) gs
  BtnNewMap     -> startMatch table (mapId gs) gs
  BtnChangeMap  -> gs { screen = MapSelectScreen }
  BtnMainMenu   -> toMenu gs
  BtnQuit       -> toMenu gs
  BtnPause      -> togglePause gs
  BtnResume     -> togglePause gs
