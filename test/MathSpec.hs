module MathSpec (spec) where

import Test.Hspec
import Test.QuickCheck (property, forAll, choose)

import Engine.Math (degToRad, vecAdd)
import Model.Types (Vec2(..))

import Engine.ParallelPhysics
  ( updatePhysicsSequential
  , updatePhysicsParallel
  )

import Model.Types
import Model.World (initGameState)

spec :: Spec
spec = describe "Engine.Math" $ do

  describe "degToRad" $ do

    it "0 do bang 0 radian" $ do
      degToRad 0 `shouldBe` 0

    it "180 do bang pi radian" $ do
      degToRad 180 `shouldSatisfy`
        (\x -> abs (x - pi) < 1e-5)

    it "90 do bang pi/2 radian" $ do
      degToRad 90 `shouldSatisfy`
        (\x -> abs (x - pi / 2) < 1e-5)

  describe "vecAdd" $ do

    it "cong hai vector" $ do
      vecAdd (Vec2 1 2) (Vec2 3 4)
        `shouldBe` Vec2 4 6

    it "tinh giao hoan cua phep cong vector" $
      property $
        forAll (choose (-1000, 1000)) $ \a ->
        forAll (choose (-1000, 1000)) $ \b ->
        forAll (choose (-1000, 1000)) $ \c ->
        forAll (choose (-1000, 1000)) $ \d ->
          vecAdd (Vec2 a b) (Vec2 c d)
            == vecAdd (Vec2 c d) (Vec2 a b)

  describe "Sequential vs Parallel Physics" $ do
    it "cho ket qua giong nhau" $ do
      let gs = initGameState []
          sequential = updatePhysicsSequential gs
          parallel = updatePhysicsParallel gs

      sequential `shouldBe` parallel