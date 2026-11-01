-- Chuyển trạng thái thuần túy: một frame của game
--
-- Thứ tự mỗi frame: di chuyển xe (kèm hiệu ứng sàn) -> giảm hồi chiêu -> vụ nổ lớn dần
-- -> vật lý đạn/tường/thùng nổ (song song) -> kiểm tra thắng thua

module Engine.Step
  ( stepGame
  , stepFrame
  , checkGameOver
  , driveTank
  , driveTankAt
  , driveTankWith
  , launchTank
  , launchTankWith
  ) where

import Engine.Collision            (tankBlockedAt, tankBlockedBy)
import Engine.Floors               (applyFloorsWith, onMud)
import Engine.Math                 (degToRad, vecAdd)
import Engine.ParallelPhysics      (updatePhysicsParallel)
import Model.Config
import Model.Types
import Model.Hp (isDead)

stepGame :: GameState -> GameState
stepGame gs
  | screen gs /= PlayScreen = gs
  | otherwise = checkGameOver (updatePhysicsParallel (tickExplosions (tickCooldowns (moveTanks gs))))

-- Cập nhật một frame của trận đấu hai người chơi
stepFrame :: GameState -> GameState
stepFrame = stepGame

-- Giảm hồi chiêu bắn mỗi frame (không xuống dưới 0)
tickCooldowns :: GameState -> GameState
tickCooldowns gs = gs { cooldown1 = max 0 (cooldown1 gs - 1)
                      , cooldown2 = max 0 (cooldown2 gs - 1) }

-- Vụ nổ lớn dần mỗi frame và biến mất khi đạt bán kính tối đa
tickExplosions :: GameState -> GameState
tickExplosions gs = gs
  { explosions = [ e { expRadius = expRadius e + explosionGrowth }
                 | e <- explosions gs
                 , expRadius e + explosionGrowth < explosionMaxRadius ] }

-- Di chuyển xe tăng: tiến/lùi/xoay, áp dụng hiệu ứng sàn (bùn, bẫy, lò xo, cổng dịch chuyển)
moveTanks :: GameState -> GameState
moveTanks gs = gs { tank1 = t1', tank2 = t2' }
  where
    ws   = walls gs
    fs   = floors gs
    keys = activeKeys gs
    t1'  = stepTank ws (tank2 gs) fs (P1Forward, P1Back, P1Left, P1Right) keys (tank1 gs)
    t2'  = stepTank ws t1'        fs (P2Forward, P2Back, P2Left, P2Right) keys (tank2 gs)

-- Nhóm bốn phím của một người chơi: tiến, lùi, quay trái, quay phải
type Pad = (Control, Control, Control, Control)

-- Một frame của một xe: choáng thì đứng yên, đang bị lò xo bắn thì bay thẳng,
-- ngược lại lái bình thường (chậm hơn trên bùn); sau đó áp dụng hiệu ứng sàn tại vị trí
-- cuối cùng. @other@ là xe còn lại, được coi như vật cản.
stepTank :: [Wall] -> Tank -> [Floor] -> Pad -> [Control] -> Tank -> Tank
stepTank ws other fs (fwd, back, lft, rgt) keys t =
  applyFloorsWith ws (Just other) fs moved
  where
    blocked = tankBlockedBy ws other
    st = tankStatus t
    moved
      | stunFrames st > 0   = t { tankStatus = st { stunFrames = stunFrames st - 1 } }
      | launchFrames st > 0 = launchTankWith blocked t
      | otherwise           = driveTankWith blocked factor fwd back lft rgt keys t
    factor = if onMud fs (tankPos t) then mudFactor else 1

-- Bị lò xo bắn: bay thẳng theo hướng nòng, đụng vật cản thì dừng bay
launchTank :: [Wall] -> Tank -> Tank
launchTank ws = launchTankWith (tankBlockedAt ws)

-- kiểm tra xem xe có bị chặn bởi tường, biên, hay xe kia không
launchTankWith :: (Vec2 -> Bool) -> Tank -> Tank
launchTankWith blocked t
  | blocked cand = t { tankStatus = st { launchFrames = 0 } }
  | otherwise    = t { tankPos    = cand
                     , tankStatus = st { launchFrames = launchFrames st - 1 } }
  where
    st   = tankStatus t
    a    = degToRad (tankAngle t)
    cand = vecAdd (tankPos t) (Vec2 (cos a * springSpeed) (sin a * springSpeed))

-- Xe tiến/lùi/xoay theo các phím đang giữ, gặp tường hoặc biên thì đứng yên
driveTank :: [Wall] -> Control -> Control -> Control -> Control -> [Control] -> Tank -> Tank
driveTank = driveTankAt 1

-- giống driveTank nhưng có thể truyền thêm hệ số tốc độ (dùng cho bùn)
driveTankAt :: Float -> [Wall] -> Control -> Control -> Control -> Control -> [Control] -> Tank -> Tank
driveTankAt factor ws = driveTankWith (tankBlockedAt ws) factor

driveTankWith
  :: (Vec2 -> Bool) -> Float -> Control -> Control -> Control -> Control -> [Control] -> Tank -> Tank
driveTankWith blocked factor fwd back lft rgt keys t =
  t { tankAngle = angle', tankPos = pos' }
  where
    held c  = c `elem` keys
    angle'  = tankAngle t
              + (if held lft then tankTurnSpeed else 0)
              - (if held rgt then tankTurnSpeed else 0)
    a       = degToRad angle'
    stepLen = factor * ((if held fwd then tankSpeed else 0) - (if held back then tankSpeed else 0))
    here    = tankPos t
    cand    = vecAdd here (Vec2 (cos a * stepLen) (sin a * stepLen))
    pos'
      | stepLen == 0 || blocked cand = here
      | otherwise                    = cand

-- Hết máu => sang GameOverScreen (cả hai cùng hết máu = hòa)
checkGameOver :: GameState -> GameState
checkGameOver gs
  | dead1 && dead2 = gs { screen = GameOverScreen Draw }
  | dead1          = gs { screen = GameOverScreen (Winner P2) }
  | dead2          = gs { screen = GameOverScreen (Winner P1) }
  | otherwise      = gs
  where
    dead1 = isDead (tankHp (tank1 gs))
    dead2 = isDead (tankHp (tank2 gs))