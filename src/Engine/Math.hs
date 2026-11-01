-- Hình học 2D thuần túy
module Engine.Math (distSq, degToRad, vecAdd) where

import Model.Types (Vec2 (..))

-- Bình phương khoảng cách (tránh sqrt)
distSq :: Vec2 -> Vec2 -> Float
distSq (Vec2 x1 y1) (Vec2 x2 y2) = dx * dx + dy * dy
  where dx = x1 - x2
        dy = y1 - y2

degToRad :: Float -> Float
degToRad d = d * pi / 180

vecAdd :: Vec2 -> Vec2 -> Vec2
vecAdd (Vec2 a b) (Vec2 c d) = Vec2 (a + c) (b + d)
