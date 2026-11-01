-- Kiểm tra va chạm hình học (thuần túy)
module Engine.Collision
  ( circlesCollide
  , bulletHitsWall
  , bulletHitsTank
  , outOfBounds
  , tankHitsWall
  , tankBlockedAt
  , tankHitsTankAt
  , tankBlockedBy
  ) where

import Engine.Math  (distSq)
import Model.Config
import Model.Types

-- Hai hình tròn va chạm khi khoảng cách < tổng bán kính (so sánh bình phương)
circlesCollide :: Vec2 -> Float -> Vec2 -> Float -> Bool
circlesCollide p1 r1 p2 r2 = distSq p1 p2 < (r1 + r2) * (r1 + r2)

-- Đạn (hình tròn) chạm khối vật cản (hình vuông, AABB)
bulletHitsWall :: Bullet -> Wall -> Bool
bulletHitsWall b w =
  abs (vx bp - vx wp) <= wallHalfSize + bulletRadius &&
  abs (vy bp - vy wp) <= wallHalfSize + bulletRadius
  where bp = bulletPos b
        wp = wallPos w

-- Đạn chạm xe tăng của người chơi. Đạn của chính chủ không tự kích nổ
bulletHitsTank :: Bullet -> PlayerId -> Tank -> Bool
bulletHitsTank b n t =
  bulletOwner b /= n &&
  circlesCollide (bulletPos b) bulletRadius (tankPos t) tankRadius

-- Ra khỏi sân (sân rộng 960 x 540, tâm ở gốc tọa độ)
outOfBounds :: Vec2 -> Bool
outOfBounds (Vec2 x y) = abs x > arenaHalfW || abs y > arenaHalfH

-- Xe đặt tại @p@ chạm vật cản (hình vuông, AABB)
tankHitsWall :: Vec2 -> Wall -> Bool
tankHitsWall (Vec2 x y) w =
  abs (x - vx wp) <= wallHalfSize + tankBlockHalf &&
  abs (y - vy wp) <= wallHalfSize + tankBlockHalf
  where wp = wallPos w

-- Vị trí bị chặn: ra khỏi sân hoặc đè lên vật cản
tankBlockedAt :: [Wall] -> Vec2 -> Bool
tankBlockedAt ws p = outOfBounds p || any (tankHitsWall p) ws


-- Xe đặt tại @p@ chạm xe kia (hai hình tròn bán kính 'tankRadius', cùng cỡ vùng trúng đạn)
tankHitsTankAt :: Vec2 -> Tank -> Bool
tankHitsTankAt p other = circlesCollide p tankRadius (tankPos other) tankRadius

-- Vị trí bị chặn bởi vật cản hoặc xe kia
tankBlockedBy :: [Wall] -> Tank -> Vec2 -> Bool
tankBlockedBy ws other p = tankBlockedAt ws p || tankHitsTankAt p other