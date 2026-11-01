module CollisionSpec (spec) where

import Test.Hspec

import Engine.Collision
import Model.Config
import Model.Types

spec :: Spec
spec = describe "Collision" $ do

  it "va cham khi khoang cach nho hon tong ban kinh" $
    circlesCollide (Vec2 0 0) 3 (Vec2 5 0) 3 `shouldBe` True

  it "khong va cham khi hai vat o xa nhau" $
    circlesCollide (Vec2 0 0) 3 (Vec2 20 0) 3 `shouldBe` False

  it "khong va cham khi khoang cach bang tong ban kinh" $
    circlesCollide (Vec2 0 0) 3 (Vec2 6 0) 3 `shouldBe` False

  it "dan cua P1 khong tu gay sat thuong cho P1" $
    bulletHitsTank (Bullet (Vec2 0 0) (Vec2 0 0) P1 True) P1 (mkTank (Vec2 0 0) 0 100)
      `shouldBe` False

  it "dan cua P1 trung xe P2" $
    bulletHitsTank (Bullet (Vec2 0 0) (Vec2 0 0) P1 True) P2 (mkTank (Vec2 5 0) 0 100)
      `shouldBe` True

  it "dan trung tuong khi nam trong khoi tuong" $
    bulletHitsWall (Bullet (Vec2 10 0) (Vec2 0 0) P1 True) (Wall (Vec2 0 0) SolidSteel 999)
      `shouldBe` True

  it "dan o xa tuong thi khong trung" $
    bulletHitsWall (Bullet (Vec2 100 0) (Vec2 0 0) P1 True) (Wall (Vec2 0 0) SolidSteel 999)
      `shouldBe` False

  it "outOfBounds: trong san 960x540 la False, vuot bien la True" $ do
    outOfBounds (Vec2 0 0)       `shouldBe` False
    outOfBounds (Vec2 480 270)   `shouldBe` False
    outOfBounds (Vec2 481 0)     `shouldBe` True
    outOfBounds (Vec2 0 (-271))  `shouldBe` True

  it "xe cham tuong khi hinh vuong cua xe chong len khoi tuong" $ do
    let w = Wall (Vec2 25 0) SolidSteel steelHp
    tankHitsWall (Vec2 0 0) w `shouldBe` True    -- sat nhau: 15 + 10 = 25
    tankHitsWall (Vec2 (-1) 0) w `shouldBe` False

  it "tankBlockedAt: ngoai san hoac de len tuong thi bi chan" $ do
    let ws = [Wall (Vec2 100 0) BreakableBrick brickMaxHp]
    tankBlockedAt ws (Vec2 0 0)     `shouldBe` False
    tankBlockedAt ws (Vec2 90 0)    `shouldBe` True
    tankBlockedAt ws (Vec2 500 0)   `shouldBe` True
