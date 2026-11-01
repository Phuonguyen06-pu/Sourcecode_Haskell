module DescribableSpec (spec) where

import Test.Hspec hiding (describe)
import qualified Test.Hspec as Hspec (describe)
import Model.Describable
import Model.Types

spec :: Spec
spec = Hspec.describe "Describable - Mo ta du lieu game" $ do

  it "Mo ta Player 1 va Player 2" $ do
    describe P1 `shouldBe` "Player 1"
    describe P2 `shouldBe` "Player 2"

  it "Mo ta cac ban do MapId" $ do
    describe Map1 `shouldBe` "Map 1"
    describe Map2 `shouldBe` "Map 2"
    describe Map3 `shouldBe` "Map 3"

    describeAll [Map1, Map2, Map3]
      `shouldBe` "Map 1 | Map 2 | Map 3"

  it "Mo ta nguoi chien thang va hoa" $ do
    describe (Winner P1) `shouldBe` "Player 1 wins"
    describe (Winner P2) `shouldBe` "Player 2 wins"
    describe Draw `shouldBe` "Draw"

  it "Mo ta vi tri Vec2" $ do
    describe (Vec2 10 20) `shouldBe` "(10,20)"

  it "Mo ta xe tang" $ do
    let tank = mkTank (Vec2 100 50) 90 80
    describe tank `shouldBe` "HP 80 at (100,50)"

  it "Mo ta loai tuong" $ do
    describe SolidSteel `shouldBe` "steel"
    describe BreakableBrick `shouldBe` "brick"
    describe ExplosiveBarrel `shouldBe` "barrel"

  it "Mo ta san dac biet" $ do
    describe Mud `shouldBe` "mud"
    describe Trap `shouldBe` "trap"
    describe Spring `shouldBe` "spring"
    describe (Teleporter (Vec2 1 2)) `shouldBe` "teleporter to (1,2)"
    describe (Floor (Vec2 30 60) Trap) `shouldBe` "trap at (30,60)"

  it "Mo ta tuong" $ do
    let wall = Wall (Vec2 50 100) BreakableBrick 60
    describe wall `shouldBe` "brick 60hp at (50,100)"

  it "Mo ta cac man hinh" $ do
    describe MenuScreen `shouldBe` "Menu"
    describe MapSelectScreen `shouldBe` "Select map"
    describe HelpScreen `shouldBe` "Help"
    describe PlayScreen `shouldBe` "Playing"
    describe PauseScreen `shouldBe` "Paused"
    describe (GameOverScreen (Winner P1))
      `shouldBe` "Game over: Player 1 wins"

  it "Mo ta ket qua va cham" $ do
    describe NoHit `shouldBe` "miss"
    describe (HitWall (WallIx 3)) `shouldBe` "wall #3"
    describe (HitTank P2) `shouldBe` "Player 2 hit"

  it "Mo ta thong ke tran dau" $ do
    let s = MatchStats 5 3 2 1 4
    describe s `shouldBe`
      "SHOTS 5/3   HITS 1/2   WALLS 4"

  it "Mo ta thong ke rong" $ do
    describe (mempty :: MatchStats)
      `shouldBe` "SHOTS 0/0   HITS 0/0   WALLS 0"

  it "Mo ta danh sach bang describeAll" $ do
    describeAll [P1, P2]
      `shouldBe` "Player 1 | Player 2"

  it "Tao dong log bang logLine" $ do
    logLine "result" (Winner P1)
      `shouldBe` "[result] Player 1 wins"