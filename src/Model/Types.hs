{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
-- Các kiểu dữ liệu đại số (ADT) thuần túy của trò chơi
--
--   'PlayerId'  chỉ có hai giá trị hợp lệ: P1, P2
--   'Winner'    kết quả trận đấu: P1 thắng, P2 thắng, hòa
--   'MapId'     chỉ có ba giá trị hợp lệ: Map1, Map2, Map3
--   'Hit'       kết quả va chạm của một viên đạn
--   'WallType' / 'FloorKind': vật cản (chặn đường) tách khỏi sàn (hiệu ứng khi đứng lên)
--   'Status'    hiệu ứng tạm thời của xe (choáng, bay, hồi chiêu dịch chuyển)
module Model.Types where

import Control.DeepSeq (NFData)
import Data.Word       (Word32)
import GHC.Generics    (Generic)
import Model.Hp (Hp, mkHp)

data Vec2 = Vec2 { vx :: !Float, vy :: !Float }
  deriving (Show, Eq, Generic, NFData)

-- Người chơi. Chỉ có đúng hai giá trị hợp lệ
data PlayerId = P1 | P2
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Generic, NFData)

-- Mã bản đồ: chỉ có ba giá trị hợp lệ, dùng để chọn file map và lưu kết quả trận đấu
data MapId = Map1 | Map2 | Map3
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Generic, NFData)

mapNumber :: MapId -> Int
mapNumber m = fromEnum m + 1

mapFromNumber :: Int -> Maybe MapId
mapFromNumber n =
  lookup n [(mapNumber m, m) | m <- [minBound .. maxBound]]

-- Kết quả trận đấu
data Winner = Winner PlayerId | Draw
  deriving (Show, Read, Eq, Generic, NFData)

newtype Seed = Seed Word32
  deriving (Show, Eq, Generic, NFData)

-- chỉ số vật cản trong danh sách walls của GameState, dùng để đánh dấu đạn trúng tường
newtype WallIx = WallIx Int
  deriving (Show, Eq, Ord, Generic, NFData)

-- Hiệu ứng tạm thời trên xe tăng (do bẫy, lò xo, dịch chuyển)
data Status = Status
  { stunFrames   :: !Int    -- còn bị choáng bao nhiêu frame (không điều khiển được)
  , launchFrames :: !Int    -- còn bị lò xo bắn bay bao nhiêu frame
  , teleCooldown :: !Int    -- hồi chiêu dịch chuyển (tránh nhảy qua lại liên tục)
  , onTrap       :: !Bool   -- frame trước đang đứng trên bẫy (chỉ trừ máu khi BƯỚC VÀO)
  , onSpring     :: !Bool   -- frame trước đang đứng trên lò xo
  } deriving (Show, Eq, Generic, NFData)

noStatus :: Status
noStatus = Status 0 0 0 False False

data Tank = Tank
  { tankPos    :: Vec2
  , tankAngle  :: Float   -- độ
  , tankHp     :: Hp
  , tankStatus :: Status
  } deriving (Show, Eq, Generic, NFData)

-- Xe tăng không có hiệu ứng nào
mkTank :: Vec2 -> Float -> Int -> Tank
mkTank p a hp = Tank p a (mkHp hp) noStatus

data Bullet = Bullet
  { bulletPos   :: Vec2
  , bulletVel   :: Vec2
  , bulletOwner :: PlayerId
  , bulletAlive :: Bool
  } deriving (Show, Eq, Generic, NFData)

-- Vật cản: chặn xe và đạn
data WallType = SolidSteel | BreakableBrick | ExplosiveBarrel
  deriving (Show, Eq, Generic, NFData)

data Wall = Wall
  { wallPos  :: Vec2
  , wallType :: WallType
  , wallHp   :: Int
  } deriving (Show, Eq, Generic, NFData)

-- Sàn đặc biệt: không chặn đường, chỉ gây hiệu ứng khi xe đứng lên
data FloorKind
  = Mud                 -- bùn lầy: giảm tốc
  | Trap                -- bẫy điện từ: trừ máu và làm choáng
  | Spring              -- lò xo: bắn xe bay theo hướng đang quay
  | Teleporter Vec2     -- cổng dịch chuyển, kèm tọa độ cổng còn lại
  deriving (Show, Eq, Generic, NFData)

data Floor = Floor
  { floorPos  :: Vec2
  , floorKind :: FloorKind
  } deriving (Show, Eq, Generic, NFData)

-- Vụ nổ đang hiển thị (hiệu ứng hình ảnh, lớn dần rồi tắt)
data Explosion = Explosion
  { expPos    :: Vec2
  , expRadius :: !Float
  } deriving (Show, Eq, Generic, NFData)

-- Bản đồ đọc từ file: phần cố định và danh sách ô "ngẫu nhiên"
data MapLayout = MapLayout
  { layoutWalls  :: [Wall]
  , layoutFloors :: [Floor]
  , layoutRandom :: [Vec2]
  } deriving (Show, Eq, Generic, NFData)

data ScreenState
  = MenuScreen
  | MapSelectScreen
  | HelpScreen
  | PlayScreen
  | PauseScreen
  | GameOverScreen Winner
  deriving (Show, Eq, Generic, NFData)

-- Phím điều khiển 
data Control
  = P1Forward | P1Back | P1Left | P1Right | P1Fire
  | P2Forward | P2Back | P2Left | P2Right | P2Fire
  deriving (Show, Eq, Ord, Enum, Bounded, Generic, NFData)

-- Kết quả va chạm của một viên đạn trong một frame
data Hit
  = NoHit
  | HitWall WallIx        -- chỉ số khối tường bị trúng
  | HitTank PlayerId   -- xe của người chơi nào bị trúng
  deriving (Show, Eq, Generic, NFData)

-- Thống kê trận đấu: số lần bắn, số lần trúng, số lần đạn trúng vật cản
data MatchStats = MatchStats
  { shotsP1  :: !Int   
  , shotsP2  :: !Int
  , hitsOnP1 :: !Int   
  , hitsOnP2 :: !Int
  , wallHits :: !Int   
  } deriving (Show, Read, Eq, Generic, NFData)

instance Semigroup MatchStats where
  MatchStats a b c d e <> MatchStats a' b' c' d' e' =
    MatchStats (a + a') (b + b') (c + c') (d + d') (e + e')

instance Monoid MatchStats where
  mempty = MatchStats 0 0 0 0 0

shotStats :: PlayerId -> MatchStats
shotStats P1 = mempty { shotsP1 = 1 }
shotStats P2 = mempty { shotsP2 = 1 }

hitStats :: Hit -> MatchStats
hitStats NoHit         = mempty
hitStats (HitWall _)   = mempty { wallHits = 1 }
hitStats (HitTank P1)  = mempty { hitsOnP1 = 1 }
hitStats (HitTank P2)  = mempty { hitsOnP2 = 1 }

-- Kết quả một trận, được lưu file và gửi qua socket
data MatchResult = MatchResult
  { mrMap     :: MapId
  , mrWinner  :: Winner
  , mrSeconds :: Int
  } deriving (Show, Read, Eq, Generic, NFData)

data GameState = GameState
  { screen     :: ScreenState
  , tank1      :: Tank
  , tank2      :: Tank
  , bullets    :: [Bullet]
  , walls      :: [Wall]
  , floors     :: [Floor]
  , explosions :: [Explosion]
  , activeKeys :: [Control]   -- các phím đang giữ
  , mapId      :: MapId 
  , cooldown1  :: !Int        -- số frame còn lại trước khi P1 bắn được
  , cooldown2  :: !Int
  , matchTime  :: !Int        -- giây; chỉ luồng đồng hồ (Concurrency.Workers) ghi
  , stats      :: MatchStats
  , resultSent :: Bool        -- luồng ghi kết quả đã xử lý trận này chưa
  , rngSeed    :: Seed        -- hạt giống cho lần tạo bản đồ ngẫu nhiên kế tiếp
  , notice     :: Maybe String  -- Thong bao hien thi tren giao dien
  } deriving (Show, Eq, Generic, NFData)
