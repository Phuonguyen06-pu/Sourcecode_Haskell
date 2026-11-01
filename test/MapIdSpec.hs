module MapIdSpec (spec) where

import Test.Hspec
import Model.Types

spec :: Spec
spec = describe "MapId" $ do

  describe "mapNumber" $ do
    it "converts Map1 to 1" $
      mapNumber Map1 `shouldBe` 1

    it "converts Map2 to 2" $
      mapNumber Map2 `shouldBe` 2

    it "converts Map3 to 3" $
      mapNumber Map3 `shouldBe` 3

  describe "mapFromNumber" $ do
    it "accepts valid map IDs" $ do
      mapFromNumber 1 `shouldBe` Just Map1
      mapFromNumber 2 `shouldBe` Just Map2
      mapFromNumber 3 `shouldBe` Just Map3

    it "rejects invalid map IDs" $ do
      mapFromNumber 0 `shouldBe` Nothing
      mapFromNumber 4 `shouldBe` Nothing
      mapFromNumber (-1) `shouldBe` Nothing

  it "converts MapId to Int and back correctly" $ do
    mapFromNumber (mapNumber Map1) `shouldBe` Just Map1
    mapFromNumber (mapNumber Map2) `shouldBe` Just Map2
    mapFromNumber (mapNumber Map3) `shouldBe` Just Map3