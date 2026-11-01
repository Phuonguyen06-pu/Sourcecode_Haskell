module ClockSpec (spec) where

import Test.Hspec
import Concurrency.StateSync
import Model.Types
import Model.World (initGameState)

spec :: Spec
spec = describe "Clock - Dong ho tran dau" $ do

  it "Dong ho tang 1 giay khi dang choi" $ do
    let gs = (initGameState [])
               { screen = PlayScreen
               , matchTime = 10
               }
        gs' = tickClock gs

    matchTime gs' `shouldBe` 11

  it "Dong ho khong tang khi o Menu" $ do
    let gs = (initGameState [])
               { screen = MenuScreen
               , matchTime = 5
               }
        gs' = tickClock gs

    matchTime gs' `shouldBe` 5

  it "Dong ho khong tang khi chon map" $ do
    let gs = (initGameState [])
               { screen = MapSelectScreen
               , matchTime = 5
               }
        gs' = tickClock gs

    matchTime gs' `shouldBe` 5

  it "Dong ho khong tang khi xem HELP" $ do
    let gs = (initGameState [])
               { screen = HelpScreen
               , matchTime = 5
               }
        gs' = tickClock gs

    matchTime gs' `shouldBe` 5

  it "Dong ho dung khi Player 1 thang" $ do
    let gs = (initGameState [])
               { screen = GameOverScreen (Winner P1)
               , matchTime = 30
               }
        gs' = tickClock gs

    matchTime gs' `shouldBe` 30

  it "Dong ho dung khi tran dau hoa" $ do
    let gs = (initGameState [])
               { screen = GameOverScreen Draw
               , matchTime = 40
               }
        gs' = tickClock gs

    matchTime gs' `shouldBe` 40

  it "Dong ho tang dung sau nhieu lan cap nhat" $ do
    let gs = (initGameState [])
               { screen = PlayScreen
               , matchTime = 0
               }
        gs' = iterate tickClock gs !! 60

    matchTime gs' `shouldBe` 60