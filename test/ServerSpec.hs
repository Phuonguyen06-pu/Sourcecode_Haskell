module ServerSpec (spec) where

import Test.Hspec

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (finally)
import Control.Monad (forM, forM_)
import Data.List (isPrefixOf)
import System.Directory (doesFileExist, removeFile)
import System.Timeout (timeout)

import Net.Client (sendCommand)
import Net.Server (ConnLimit(..), handleCommand, serveOn)
import Support (freePort)
import Parser.Protocol (Command(..))
import Storage.Save (Scoreboard(..), SaveData(..), loadSave)
import Model.Types (MatchResult(..), PlayerId(..), Winner(..), MapId(..))
spec :: Spec
spec = describe "Net.Server.handleCommand" $ do

  it "QueryStats tra ve bang diem ma khong thay doi du lieu" $ do
    let board = Scoreboard 2 3 1
        (reply, updated) = handleCommand QueryStats board

    reply `shouldBe` "P1=2 P2=3 DRAW=1"
    updated `shouldBe` board

  it "Report P1 tang so tran thang cua P1" $ do
    let board = Scoreboard 0 0 0
        result = MatchResult Map1 (Winner P1) 30
        (reply, updated) = handleCommand (Report result) board

    reply `shouldBe` "OK P1=1 P2=0 DRAW=0"
    updated `shouldBe` Scoreboard 1 0 0

  it "Report P2 tang so tran thang cua P2" $ do
    let board = Scoreboard 0 0 0
        result = MatchResult Map2 (Winner P2) 45
        (reply, updated) = handleCommand (Report result) board

    reply `shouldBe` "OK P1=0 P2=1 DRAW=0"
    updated `shouldBe` Scoreboard 0 1 0

  it "Report Draw tang so tran hoa" $ do
    let board = Scoreboard 0 0 0
        result = MatchResult Map3 Draw 60
        (reply, updated) = handleCommand (Report result) board

    reply `shouldBe` "OK P1=0 P2=0 DRAW=1"
    updated `shouldBe` Scoreboard 0 0 1

  it "Nhieu ket qua duoc cong don chinh xac" $ do
    let board = Scoreboard 2 1 1
        result = MatchResult Map1 (Winner P1) 20
        (_, updated) = handleCommand (Report result) board

    updated `shouldBe` Scoreboard 3 1 1

  it "10 client dong thoi khong lam mat ket qua" $ do
    let testFile = "server-concurrent-test.txt"
        clientCount = 10

        cleanup = do
          exists <- doesFileExist testFile
          if exists then removeFile testFile else pure ()

        runTest = do
          cleanup

          testPort <- freePort

          serverDone <- newEmptyMVar

          _ <- forkIO $ do
            serveOn (Exactly clientCount) testPort testFile
              `finally` putMVar serverDone ()

          -- Cho server khoi dong
          threadDelay 500000

          -- Tao 10 client gui ket qua dong thoi
          clientVars <- forM [1 .. clientCount] $ \i -> do
            done <- newEmptyMVar

            _ <- forkIO $ do
              let result = MatchResult Map1 (Winner P1) i
              reply <- sendCommand "127.0.0.1" testPort (Report result)
              putMVar done reply

            pure done

          -- Kiem tra phan hoi cua 10 client
          replies <- mapM takeMVar clientVars

          forM_ replies $ \reply ->
            case reply of
              Right msg ->
                msg `shouldSatisfy` ("OK " `isPrefixOf`)

              Left err ->
                expectationFailure ("Client error: " ++ show err)

          -- Doi server xu ly xong
          takeMVar serverDone

          -- Kiem tra bang diem duoc luu
          saved <- loadSave testFile

          case saved of
            Right sd ->
              sdBoard sd `shouldBe` Scoreboard 10 0 0

            Left err ->
              expectationFailure ("Save error: " ++ show err)

    outcome <- timeout (15 * 1000000) (runTest `finally` cleanup)

    case outcome of
      Nothing -> expectationFailure "Concurrent test timed out"
      Just () -> pure ()