module HpSpec (spec) where

import Test.Hspec
import Model.Hp (mkHp, hpValue, damageHp, isDead)

spec :: Spec
spec = describe "Hp khong am" $ do

  it "HP am khi khoi tao duoc dua ve 0" $
    hpValue (mkHp (-10)) `shouldBe` 0

  it "Sat thuong khong lam HP xuong duoi 0" $
    hpValue (damageHp 150 (mkHp 100)) `shouldBe` 0

  it "Tru mau binh thuong" $
    hpValue (damageHp 20 (mkHp 100)) `shouldBe` 80

  it "HP bang 0 thi xe chet" $
    isDead (mkHp 0) `shouldBe` True

  it "HP lon hon 0 thi xe con song" $
    isDead (mkHp 50) `shouldBe` False