module StateSyncSpec (spec) where

import Control.Concurrent (forkIO)
import Control.Concurrent.Async (async, wait)
import Control.Monad (replicateM_)
import Engine.Step (stepFrame)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.STM (atomically)
import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Control.Monad (forM, replicateM)
import System.Timeout (timeout)
import Test.Hspec

import Concurrency.StateSync
import Model.Types
import Model.World (initGameState)

-- Trang thai mau dung cho kiem thu
testGameState :: GameState
testGameState = (initGameState []) { screen = PlayScreen }

spec :: Spec
spec = describe "Concurrency.StateSync" $ do

  it "100 luong cung tang matchTime, ket qua bang 100" $ do
    world <- initSharedWorld testGameState

    signals <- replicateM 100 newEmptyMVar

    mapM_ (\signal ->
      forkIO $ do
        mutateWorld world
          (\gs -> gs { matchTime = matchTime gs + 1 })
        putMVar signal ()
      ) signals

    mapM_ takeMVar signals

    final <- readWorld world
    matchTime final `shouldBe` 100

  it "awaitResult chi tra ket qua mot lan khi nhieu luong cung cho" $ do
    world <- initSharedWorld testGameState

    signals <- replicateM 10 newEmptyMVar

    -- Tao 10 luong cung cho ket qua tran dau
    mapM_ (\signal ->
      forkIO $ do
        result <- timeout 1000000 (atomically (awaitResult world))
        putMVar signal result
      ) signals

    -- Chuyen trang thai sang ket thuc tran
    mutateWorld world $ \gs ->
      gs { screen = GameOverScreen (Winner P1)
         , matchTime = 25
         , resultSent = False
         }

    results <- mapM takeMVar signals

    let expected = MatchResult Map1 (Winner P1) 25
        successful = length (filter (== Just expected) results)

    successful `shouldBe` 1

    final <- readWorld world
    resultSent final `shouldBe` True

  it "mutateWorld bat thunk loi ngay, TVar khong bi nhiem" $ do
    world <- initSharedWorld testGameState

    before <- readWorld world

    mutateWorld world
      (\gs -> gs { bullets = [error "boom"] })
      `shouldThrow` anyErrorCall

    after <- readWorld world

    after `shouldBe` before

    _ <- evaluate (force after)

    pure ()

  it "stepFrame va tickClock chay dong thoi khong mat cap nhat" $ do
    let n = 2000

    world <- initSharedWorld testGameState
      { cooldown1 = n
      , cooldown2 = n
      }

    a <- async (replicateM_ n (mutateWorld world stepFrame))
    b <- async (replicateM_ n (mutateWorld world tickClock))

    wait a
    wait b

    final <- readWorld world

    matchTime final `shouldBe` n
    cooldown1 final `shouldBe` 0
    cooldown2 final `shouldBe` 0
    screen final `shouldBe` PlayScreen