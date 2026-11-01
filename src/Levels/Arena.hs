-- Dựng sân đấu cho một trận từ bản đồ đọc từ file 
--
-- File map chứa phần cố định (thép, gạch, thùng nổ, bùn, bẫy, lò xo, cổng) và các ô '?'
-- Mỗi trận mới, mỗi ô '?' được quyết định ngẫu nhiên là thép / gạch / trống

module Levels.Arena
  ( MapTable
  , buildArena
  , randomCell
  ) where

import Engine.Random (nextFloats)
import Model.Config
import Model.Types
import Model.World   (wallMaxHp)

-- Bảng tra bản đồ theo số map (1..3)
type MapTable = MapId -> MapLayout

-- Trả về một ô ngẫu nhiên: thép, gạch, hay trống, dựa trên số thực [0, 1)
randomCell :: Float -> Maybe WallType
randomCell r
  | r < steelChance               = Just SolidSteel
  | r < steelChance + brickChance = Just BreakableBrick
  | otherwise                     = Nothing

-- Trả về (vật cản của trận, sàn đặc biệt, hạt giống mới)
-- Cùng hạt giống và cùng bản đồ => luôn ra cùng một sân
buildArena :: Seed -> MapLayout -> ([Wall], [Floor], Seed)
buildArena seed layout = (layoutWalls layout ++ randomWalls, layoutFloors layout, seed')
  where
    cells         = layoutRandom layout
    (rolls, seed') = nextFloats (length cells) seed
    randomWalls   = [ Wall p wt (wallMaxHp wt)
                    | (p, r) <- zip cells rolls
                    , Just wt <- [randomCell r] ]
