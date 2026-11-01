{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

-- Kiểu dữ liệu máu xe tăng, không cho phép giá trị âm
module Model.Hp
  ( Hp
  , mkHp
  , hpValue
  , damageHp
  , isDead
  ) where

import Control.DeepSeq (NFData)
import GHC.Generics (Generic)

-- Máu xe tăng, được tạo thông qua mkHp
newtype Hp = Hp Int
  deriving (Show, Eq, Ord, Generic, NFData)

-- Tạo HP, đảm bảo không âm
mkHp :: Int -> Hp
mkHp = Hp . max 0

-- Lấy giá trị HP dạng số nguyên
hpValue :: Hp -> Int
hpValue (Hp n) = n

-- Trừ máu nhưng không để HP âm
damageHp :: Int -> Hp -> Hp
damageHp d (Hp n) = mkHp (n - d)

-- Kiểm tra xe đã hết máu chưa
isDead :: Hp -> Bool
isDead (Hp n) = n <= 0