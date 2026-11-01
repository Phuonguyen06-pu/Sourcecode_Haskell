-- Bộ sinh số giả ngẫu nhiên THUẦN TÚY 

module Engine.Random
  ( nextFloat
  , nextFloats
  , seedFromInt
  ) where

import Data.Bits (shiftR)

import Model.Types (Seed (..))

seedFromInt :: Int -> Seed
seedFromInt = Seed . fromIntegral

-- Số thực trong [0, 1) và hạt giống kế tiếp
nextFloat :: Seed -> (Float, Seed)
nextFloat (Seed s) = (fromIntegral (s' `shiftR` 8) / 16777216, Seed s')
  where s' = 1664525 * s + 1013904223

-- Danh sách n số thực trong [0, 1) và hạt giống kế tiếp
nextFloats :: Int -> Seed -> ([Float], Seed)
nextFloats n s
  | n <= 0    = ([], s)
  | otherwise = (x : xs, s'')
  where
    (x, s')   = nextFloat s
    (xs, s'') = nextFloats (n - 1) s'
