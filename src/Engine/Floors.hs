-- Hiệu ứng của SÀN đặc biệt lên xe tăng
--
--   Mud        : giảm tốc ('onMud', dùng trong Engine.Step)
--   Trap       : trừ máu và làm choáng, chỉ khi BƯỚC VÀO ô bẫy
--   Spring     : bắn xe bay theo hướng đang quay, chỉ khi bước vào ô lò xo
--   Teleporter : nhảy sang cổng còn lại, có hồi chiêu

module Engine.Floors
  ( tileOf
  , floorsAt
  , onMud
  , applyFloors
  , applyFloorsWith
  ) where

import Engine.Collision (tankBlockedAt, tankHitsTankAt)
import Engine.Math      (degToRad, distSq, vecAdd)
import Model.Config
import Model.Types
import Model.Hp (damageHp)

tileOf :: Vec2 -> (Int, Int)
tileOf (Vec2 x y) =
  ( floor ((x + arenaHalfW) / tileSize)
  , floor ((arenaHalfH - y) / tileSize) )

floorsAt :: [Floor] -> Vec2 -> [FloorKind]
floorsAt fs p = [floorKind f | f <- fs, tileOf (floorPos f) == tileOf p]

onMud :: [Floor] -> Vec2 -> Bool
onMud fs p = Mud `elem` floorsAt fs p

-- Áp dụng bẫy, lò xo, cổng dịch chuyển cho một xe (sau khi xe đã di chuyển trong frame)
-- Xe đang bay (do lò xo) thì bỏ qua sàn
applyFloors :: [Wall] -> [Floor] -> Tank -> Tank
applyFloors ws = applyFloorsWith ws Nothing

-- như applyFloors nhưng có thể truyền thêm xe khác để tránh nhảy vào xe kia
applyFloorsWith :: [Wall] -> Maybe Tank -> [Floor] -> Tank -> Tank
applyFloorsWith ws other fs t
  | launchFrames (tankStatus t) > 0 = t
  | otherwise =
      teleportEffect ws other fs (springEffect standsOnSpring (trapEffect standsOnTrap t))
  where
    here           = floorsAt fs (tankPos t)
    standsOnTrap   = Trap   `elem` here
    standsOnSpring = Spring `elem` here

-- Bẫy: trừ máu và làm choáng, chỉ khi BƯỚC VÀO ô bẫy
trapEffect :: Bool -> Tank -> Tank
trapEffect standing t
  | standing && not (onTrap st) =
      t { tankHp = damageHp trapDamage (tankHp t) 
        , tankStatus = st { stunFrames = trapStunFrames, onTrap = True } 
        }
  | otherwise = t { tankStatus = st { onTrap = standing } }
  where st = tankStatus t

-- Lò xo: bắn xe bay theo hướng đang quay, chỉ khi bước vào ô lò xo
springEffect :: Bool -> Tank -> Tank
springEffect standing t
  | standing && not (onSpring st) =
      t { tankStatus = st { launchFrames = springFrames, onSpring = True } }
  | otherwise = t { tankStatus = st { onSpring = standing } }
  where st = tankStatus t

-- Cổng dịch chuyển: nhảy sang cổng còn lại, có hồi chiêu
teleportEffect :: [Wall] -> Maybe Tank -> [Floor] -> Tank -> Tank
teleportEffect ws other fs t
  | teleCooldown st > 0 = t { tankStatus = st { teleCooldown = teleCooldown st - 1 } }
  | (dest : _) <- targets
  , Just p <- landing dest =
      t { tankPos    = p
        , tankStatus = st { teleCooldown = teleportCooldownFrames } }
  | otherwise = t
  where
    st      = tankStatus t
    targets = [ d | Floor c (Teleporter d) <- fs
                  , distSq (tankPos t) c < teleportRadius * teleportRadius ]
    a       = degToRad (tankAngle t)
    ahead d = vecAdd d (Vec2 (cos a * teleportExitDist) (sin a * teleportExitDist))
    byOther p = maybe False (tankHitsTankAt p) other
    landing d
      | not (tankBlockedAt ws (ahead d) || byOther (ahead d)) = Just (ahead d)
      | not (byOther d)                                       = Just d
      | otherwise                                             = Nothing