-- chuyển đổi các kiểu dữ liệu game sang chuỗi mô tả, dùng cho log và debug

module Model.Describable (Describable (..), logLine) where

import Data.List (intercalate)

import Model.Types
import Model.Hp (hpValue)

class Describable a where
  describe :: a -> String
  describeAll :: [a] -> String
  describeAll = intercalate " | " . map describe

instance Describable PlayerId where
  describe P1 = "Player 1"
  describe P2 = "Player 2"

instance Describable MapId where
  describe m = "Map " ++ show (mapNumber m)

instance Describable Winner where
  describe (Winner p) = describe p ++ " wins"
  describe Draw       = "Draw"

instance Describable Vec2 where
  describe (Vec2 x y) = "(" ++ show (round x :: Int) ++ "," ++ show (round y :: Int) ++ ")"

instance Describable Tank where
  describe t = "HP " ++ show (hpValue (tankHp t)) ++ " at " ++ describe (tankPos t)

instance Describable WallType where
  describe SolidSteel      = "steel"
  describe BreakableBrick  = "brick"
  describe ExplosiveBarrel = "barrel"

instance Describable Wall where
  describe w = describe (wallType w) ++ " " ++ show (wallHp w) ++ "hp at " ++ describe (wallPos w)

instance Describable FloorKind where
  describe Mud            = "mud"
  describe Trap           = "trap"
  describe Spring         = "spring"
  describe (Teleporter d) = "teleporter to " ++ describe d

instance Describable Floor where
  describe f = describe (floorKind f) ++ " at " ++ describe (floorPos f)

instance Describable ScreenState where
  describe MenuScreen         = "Menu"
  describe MapSelectScreen    = "Select map"
  describe HelpScreen         = "Help"
  describe PlayScreen         = "Playing"
  describe PauseScreen        = "Paused"
  describe (GameOverScreen w) = "Game over: " ++ describe w

instance Describable Hit where
  describe NoHit       = "miss"
  describe (HitWall (WallIx i)) = "wall #" ++ show i
  describe (HitTank p) = describe p ++ " hit"

instance Describable MatchStats where
  describe s =
    "SHOTS " ++ show (shotsP1 s) ++ "/" ++ show (shotsP2 s)
    ++ "   HITS " ++ show (hitsOnP2 s) ++ "/" ++ show (hitsOnP1 s)
    ++ "   WALLS " ++ show (wallHits s)

logLine :: Describable a => String -> a -> String
logLine tag x = "[" ++ tag ++ "] " ++ describe x
