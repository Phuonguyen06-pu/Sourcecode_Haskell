-- Thùng nổ: nổ lan 5x5 ô, phá gạch, kích nổ các thùng khác trong vùng
module Engine.Explosion
  ( detonate
  , blastDamageTo
  ) where

import Engine.Math (distSq)
import Model.Config
import Model.Types

-- xử lý vụ nổ: nhận danh sách tâm vụ nổ, danh sách vật cản
-- Trả về (danh sách vật cản còn lại, mọi tâm vụ nổ gồm cả vụ nổ dây chuyền)
-- Mỗi vụ nổ phá hủy gạch và kích hoạt các thùng nổ trong vùng 5x5
-- (tường thép không bị phá hủy). Quá trình kết thúc khi không còn thùng chờ nổ
detonate :: [Vec2] -> [Wall] -> ([Wall], [Vec2])
detonate []          ws = (ws, [])
detonate (c : queue) ws = (ws', c : centers)
  where
    inBlast w = abs (vx (wallPos w) - vx c) <= blastReach
             && abs (vy (wallPos w) - vy c) <= blastReach
    survives w = not (inBlast w) || wallType w == SolidSteel
    chained    = [wallPos w | w <- ws, inBlast w, wallType w == ExplosiveBarrel]
    (ws', centers) = detonate (queue ++ chained) (filter survives ws)

-- Sát thương một xe nhận từ các vụ nổ
blastDamageTo :: [Vec2] -> Tank -> Int
blastDamageTo centers t =
  blastDamage * length [c | c <- centers, distSq (tankPos t) c < blastRadius * blastRadius]
