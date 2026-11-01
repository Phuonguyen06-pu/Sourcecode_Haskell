-- song song hóa kiểm tra va chạm bằng Control.Parallel.Strategies
--
-- Bước 1 Decomposition : mỗi viên đạn / mỗi khối tường là một bài toán độc lập
-- Bước 2 Assignment    : chia danh sách đạn và danh sách tường thành các chunk (mỗi chunk 1 spark)
-- Bước 3 Orchestration : rdeepseq ép tính toán thật sự xảy ra trong spark (không để thunk lười)
-- Bước 4 Mapping       : GHC RTS (-N4) tự phân spark cho HEC 0..3 (work stealing)

module Engine.ParallelPhysics
  ( Hit (..)
  , moveBullet
  , processBullet
  , breakDamage
  , updateBulletsParallel
  , damageWallsParallel
  , updatePhysicsSequential
  , updatePhysicsParallel
  , updatePhysicsWithChunks
  ) where

import Control.Parallel.Strategies
  ( parListChunk
  , evalList
  , rdeepseq
  , using
  )
import Data.List                   (findIndex)
import qualified Data.IntMap.Strict as IM
import Engine.Collision
import Engine.Explosion (blastDamageTo, detonate)
import Engine.Math      (vecAdd)
import Model.Config
import Model.Types
import Model.Hp (damageHp)

moveBullet :: Bullet -> Bullet
moveBullet b = b { bulletPos = vecAdd (bulletPos b) (bulletVel b) }

-- Xử lý một viên đạn: di chuyển, kiểm tra va chạm với tường, xe, ra khỏi sân
processBullet :: [Wall] -> Tank -> Tank -> Bullet -> (Bullet, Hit)
processBullet ws t1 t2 b
  | outOfBounds (bulletPos m)       = (dead, NoHit)
  | Just i <- findIndex (bulletHitsWall m) ws = (dead, HitWall (WallIx i))
  | bulletHitsTank m P1 t1          = (dead, HitTank P1)
  | bulletHitsTank m P2 t2          = (dead, HitTank P2)
  | otherwise                       = (m, NoHit)
  where
    m    = moveBullet b
    dead = m { bulletAlive = False }

-- Xử lý đạn tuần tự, dùng để so sánh benchmark
updateBulletsSequential :: [Wall] -> Tank -> Tank -> [Bullet] -> [(Bullet, Hit)]
updateBulletsSequential ws t1 t2 bs =
  map (processBullet ws t1 t2) bs
    `using` evalList rdeepseq

-- Xử lý đạn song song với chunk tùy chỉnh, dùng cho benchmark
updateBulletsWithChunk
  :: Int -> [Wall] -> Tank -> Tank -> [Bullet] -> [(Bullet, Hit)]
updateBulletsWithChunk chunk ws t1 t2 bs =
  map (processBullet ws t1 t2) bs
    `using` parListChunk (max 1 chunk) rdeepseq

-- Xử lý đạn song song với chunk mặc định
updateBulletsParallel :: [Wall] -> Tank -> Tank -> [Bullet] -> [(Bullet, Hit)]
updateBulletsParallel = updateBulletsWithChunk bulletChunkSize

-- Sát thương mỗi viên đạn gây ra cho từng loại vật cản (thép: 0, không vỡ)
breakDamage :: WallType -> Int
breakDamage SolidSteel      = 0
breakDamage BreakableBrick  = brickDamage
breakDamage ExplosiveBarrel = barrelHp

-- Đếm số lần mỗi khối tường bị trúng đạn
countWallHits :: [Hit] -> IM.IntMap Int
countWallHits = foldr countHit IM.empty
  where
    countHit (HitWall (WallIx i)) counts =
      IM.insertWith (+) i 1 counts
    countHit _ counts = counts

-- Xử lý sát thương tường tuần tự, dùng để so sánh benchmark
damageWallsSequential :: [Hit] -> [Wall] -> [Wall]
damageWallsSequential hits ws = filter ((> 0) . wallHp) updated
  where
    hitCounts = countWallHits hits

    updated =
      map damage (zip (map WallIx [0 ..]) ws)
        `using` evalList rdeepseq

    damage (WallIx i, w) =
      case breakDamage (wallType w) of
        0 -> w
        d -> w
          { wallHp = wallHp w
              - d * IM.findWithDefault 0 i hitCounts
          }

-- Xử lý tường song song với kích thước chunk tùy chỉnh
damageWallsWithChunk :: Int -> [Hit] -> [Wall] -> [Wall]
damageWallsWithChunk chunk hits ws =
  filter ((> 0) . wallHp) updated
  where
    hitCounts = countWallHits hits

    updated =
      map damage (zip (map WallIx [0 ..]) ws)
        `using` parListChunk (max 1 chunk) rdeepseq

    damage (WallIx i, w) =
      case breakDamage (wallType w) of
        0 -> w
        d -> w
          { wallHp = wallHp w
              - d * IM.findWithDefault 0 i hitCounts
          }

-- Xử lý tường song song với chunk mặc định
damageWallsParallel :: [Hit] -> [Wall] -> [Wall]
damageWallsParallel = damageWallsWithChunk wallChunkSize

-- Xử lý vật lý với chiến lược xử lý đạn và tường tùy chọn
updatePhysicsWith
  :: ([Wall] -> Tank -> Tank -> [Bullet] -> [(Bullet, Hit)])
  -> ([Hit] -> [Wall] -> [Wall])
  -> GameState
  -> GameState
updatePhysicsWith processBullets processWalls gs = gs
  { bullets    = [b | (b, _) <- results, bulletAlive b]
  , walls      = wallsAfter
  , tank1      = hurt (tank1 gs)
      (bulletDamage * count (HitTank P1)
        + blastDamageTo centers (tank1 gs))
  , tank2      = hurt (tank2 gs)
      (bulletDamage * count (HitTank P2)
        + blastDamageTo centers (tank2 gs))
  , explosions = explosions gs
      ++ [Explosion c explosionStartRadius | c <- centers]
  , stats      = stats gs <> foldMap hitStats hits
  }
  where
    live = filter bulletAlive (bullets gs)

    results =
      processBullets (walls gs) (tank1 gs) (tank2 gs) live

    hits = map snd results

    count h = length (filter (== h) hits)

    damaged = processWalls hits (walls gs)

    struck =
      [ wallPos w
      | (i, w) <- zip (map WallIx [0 ..]) (walls gs)
      , wallType w == ExplosiveBarrel
      , HitWall i `elem` hits
      ]

    (wallsAfter, centers) = detonate struck damaged

    hurt t n = t { tankHp = damageHp n (tankHp t) }


-- Tuần tự, dung để so sánh benchmark
updatePhysicsSequential :: GameState -> GameState
updatePhysicsSequential =
  updatePhysicsWith
    updateBulletsSequential
    damageWallsSequential


-- song song, dùng măc định chunk
updatePhysicsParallel :: GameState -> GameState
updatePhysicsParallel =
  updatePhysicsWith
    updateBulletsParallel
    damageWallsParallel

-- song song với chunk tùy chỉnh
updatePhysicsWithChunks :: Int -> Int -> GameState -> GameState
updatePhysicsWithChunks bulletChunk wallChunk =
  updatePhysicsWith
    (updateBulletsWithChunk bulletChunk)
    (damageWallsWithChunk wallChunk)