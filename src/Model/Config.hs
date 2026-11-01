-- Hằng số quy chuẩn của game
module Model.Config where

windowWidth, windowHeight :: Int
windowWidth  = 1000
windowHeight = 700

fps :: Int
fps = 60

-- Sân đấu: lưới 32 x 18 ô, mỗi ô 30px => 960 x 540
-- Gốc tọa độ (0,0) ở tâm sân, trục y hướng lên
tileSize :: Float
tileSize = 30

arenaCols, arenaRows :: Int
arenaCols = 32
arenaRows = 18

arenaHalfW, arenaHalfH :: Float
arenaHalfW = fromIntegral arenaCols * tileSize / 2
arenaHalfH = fromIntegral arenaRows * tileSize / 2

-- Xe tăng
tankMaxHp :: Int
tankMaxHp = 100

tankRadius :: Float       -- bán kính trúng đạn
tankRadius = 12

tankBlockHalf :: Float    -- nửa cạnh hình vuông va chạm với tường
tankBlockHalf = tankRadius - 2

tankSpeed :: Float        -- px / frame
tankSpeed = 3.0

tankTurnSpeed :: Float    -- độ / frame
tankTurnSpeed = 3.5

-- Đạn: mỗi lần bắn ra HAI viên (nòng đôi)
bulletRadius :: Float
bulletRadius = 3

bulletSpeed :: Float      -- px / frame
bulletSpeed = 8

bulletDamage :: Int
bulletDamage = 10

muzzleDistance :: Float   -- từ tâm xe tới đầu nòng
muzzleDistance = 20

barrelOffset :: Float     -- lệch ngang của mỗi nòng so với trục xe
barrelOffset = 6

-- Tường
wallHalfSize :: Float
wallHalfSize = tileSize / 2

brickMaxHp :: Int
brickMaxHp = 50

brickDamage :: Int        -- 1 viên đạn là vỡ gạch
brickDamage = 50

steelHp :: Int
steelHp = 999

barrelHp :: Int           -- thùng nổ: 1 viên là nổ
barrelHp = 1

-- Ô ngẫu nhiên khi vào trận ('?' trong file map)
steelChance, brickChance :: Float
steelChance = 0.12
brickChance = 0.10

-- Thùng nổ
blastReach :: Float       -- nổ lan 5x5 ô: lệch tối đa 2 ô (cộng dung sai)
blastReach = 2 * tileSize + 1

blastRadius :: Float      -- bán kính gây sát thương lên xe
blastRadius = 70

blastDamage :: Int
blastDamage = 40

explosionStartRadius, explosionMaxRadius, explosionGrowth :: Float
explosionStartRadius = 10
explosionMaxRadius   = 65
explosionGrowth      = 4

-- Sàn đặc biệt
mudFactor :: Float        -- bùn lầy: giảm 70% tốc độ
mudFactor = 0.3

trapDamage :: Int
trapDamage = 10

trapStunFrames :: Int
trapStunFrames = 25

springSpeed :: Float      -- px / frame khi bị lò xo bắn đi
springSpeed = 14

springFrames :: Int       -- 12 frame * 14px = 168px ~ 5-6 ô
springFrames = 12

teleportRadius :: Float
teleportRadius = 18

teleportCooldownFrames :: Int
teleportCooldownFrames = 45

teleportExitDist :: Float
teleportExitDist = 25

-- Thời gian chờ giữa hai phát bắn (frame; 60 frame = 1 giây)
fireCooldown :: Int
fireCooldown = 20

-- kích thước chunk
bulletChunkSize :: Int
bulletChunkSize = 1

wallChunkSize :: Int
wallChunkSize = 16

clientTimeoutUs :: Int
clientTimeoutUs = 3 * 1000000

serverTimeoutUs :: Int
serverTimeoutUs = 10 * 1000000

serverListenBacklog :: Int
serverListenBacklog = 16

maxLineBytes :: Int
maxLineBytes = 1024

viewScale :: Float
viewScale = 0.92