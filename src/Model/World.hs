-- Khởi tạo trạng thái ban đầu và vài hàm truy cập theo người chơi
module Model.World
  ( initGameState
  , spawnP1
  , spawnP2
  , wallMaxHp
  , tankOf
  , cooldownOf
  , setCooldown
  ) where

import Model.Config
import Model.Types

-- Điểm xuất phát (nằm trong "vùng an toàn" góc trên-trái và góc dưới-phải của sân)
spawnP1, spawnP2 :: Vec2
spawnP1 = Vec2 (-390) 120
spawnP2 = Vec2 390 (-120)

-- Máu ban đầu của từng loại vật cản
wallMaxHp :: WallType -> Int
wallMaxHp SolidSteel      = steelHp
wallMaxHp BreakableBrick  = brickMaxHp
wallMaxHp ExplosiveBarrel = barrelHp

-- Trạng thái đầu trận với danh sách tường cho trước 
initGameState :: [Wall] -> GameState
initGameState ws = GameState
  { screen     = MenuScreen
  , tank1      = mkTank spawnP1 0   tankMaxHp
  , tank2      = mkTank spawnP2 180 tankMaxHp
  , bullets    = []
  , walls      = ws
  , floors     = []
  , explosions = []
  , activeKeys = []
  , mapId      = Map1
  , cooldown1  = 0
  , cooldown2  = 0
  , matchTime  = 0
  , stats      = mempty
  , resultSent = False
  , rngSeed    = Seed 20241030
  , notice = Nothing
  }

tankOf :: PlayerId -> GameState -> Tank
tankOf P1 = tank1
tankOf P2 = tank2

cooldownOf :: PlayerId -> GameState -> Int
cooldownOf P1 = cooldown1
cooldownOf P2 = cooldown2

setCooldown :: PlayerId -> Int -> GameState -> GameState
setCooldown P1 n gs = gs { cooldown1 = n }
setCooldown P2 n gs = gs { cooldown2 = n }
