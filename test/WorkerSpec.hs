module WorkerSpec (spec) where

import Test.Hspec

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, cancel, waitCatch)
import Control.Concurrent.MVar
  ( newMVar
  , newEmptyMVar
  , putMVar
  , takeMVar
  )
import Control.Exception
  ( throwIO
  , finally
  , getMaskingState
  , MaskingState(..)
  )

import Data.IORef
  ( newIORef
  , readIORef
  , modifyIORef'
  )
import Data.Either (isLeft)
import System.Timeout (timeout)
import System.Directory (doesFileExist, removeFile)

import Concurrency.Workers
  ( supervised
  , autosaveWorker
  , resultWorker
  )

import Concurrency.StateSync
  ( initSharedWorld
  , readWorld
  )

import Model.Types
import Model.World (initGameState)

import Parser.ConfigParser
  ( RuntimeConfig(..)
  , defaultConfig
  )

import Storage.Save
  ( Scoreboard(..)
  , SaveData(..)
  , loadSave
  )

import Net.Server (ConnLimit(..), serveOn)
import Support (freePort)

spec :: Spec
spec = describe "Concurrency.Workers.supervised" $ do

  it "khoi dong lai worker sau ngoai le dong bo" $ do
    attempts <- newIORef (0 :: Int)

    let action = do
          modifyIORef' attempts (+ 1)
          n <- readIORef attempts

          if n == 1
            then throwIO (userError "test synchronous exception")
            else pure ()

    result <- timeout 3000000 (supervised "test" action)

    result `shouldBe` Just ()

    count <- readIORef attempts
    count `shouldBe` 2

  it "khong nuot ngoai le huy luong" $ do
    worker <- async $
      supervised "test" (threadDelay 10000000)

    cancel worker

    result <- waitCatch worker

    result `shouldSatisfy` isLeft

  it "worker duoc khoi dong lai o trang thai Unmasked" $ do
    attempts <- newIORef (0 :: Int)
    startedAgain <- newEmptyMVar

    let action = do
          modifyIORef' attempts (+ 1)
          n <- readIORef attempts

          if n == 1
            then throwIO (userError "first attempt failed")
            else do
              state <- getMaskingState
              putMVar startedAgain state
              threadDelay 10000000

    worker <- async (supervised "test" action)

    let runTest = do
          restarted <- timeout 3000000 (takeMVar startedAgain)

          restarted `shouldBe` Just Unmasked

          stopped <- timeout 2000000 (cancel worker)

          stopped `shouldBe` Just ()

          result <- waitCatch worker
          result `shouldSatisfy` isLeft

          count <- readIORef attempts
          count `shouldBe` 2

    runTest `finally` cancel worker

  it "autosaveWorker luu snapshot khi dang choi" $ do
    let file = "worker-autosave-test.txt"
        cfg = defaultConfig
          { cfgAutosaveSecs = 1
          , cfgSaveFile = file
          , cfgReport = Nothing
          }

        cleanup = do
          exists <- doesFileExist file
          if exists then removeFile file else pure ()

    cleanup

    let runTest = do
          world <- initSharedWorld
            ((initGameState []) { screen = PlayScreen })

          lock <- newMVar ()

          worker <- async (autosaveWorker cfg lock world)

          threadDelay 1500000

          cancel worker

          saved <- loadSave file

          case saved of
            Right sd ->
              sdLast sd `shouldSatisfy` (/= Nothing)

            Left err ->
              expectationFailure ("Autosave error: " ++ show err)

    runTest `finally` cleanup

  it "resultWorker luu ket qua tran dau mot lan" $ do
    let file = "worker-result-test.txt"
        cfg = defaultConfig
          { cfgSaveFile = file
          , cfgReport = Nothing
          }

        cleanup = do
          exists <- doesFileExist file
          if exists then removeFile file else pure ()

    cleanup

    let runTest = do
          world <- initSharedWorld
            ((initGameState [])
              { screen = GameOverScreen (Winner P1)
              , matchTime = 25
              , resultSent = False
              })

          lock <- newMVar ()

          worker <- async (resultWorker cfg lock world)

          threadDelay 500000

          cancel worker

          saved <- loadSave file

          case saved of
            Right sd ->
              sdBoard sd `shouldBe` Scoreboard 1 0 0

            Left err ->
              expectationFailure ("Result save error: " ++ show err)

          final <- readWorld world
          resultSent final `shouldBe` True

    runTest `finally` cleanup

  it "resultWorker gui ket qua den TCP server" $ do
    let localFile = "worker-tcp-local-test.txt"
        serverFile = "worker-tcp-server-test.txt"

        cleanupFile file = do
          exists <- doesFileExist file
          if exists then removeFile file else pure ()

        cleanup = do
          cleanupFile localFile
          cleanupFile serverFile

    cleanup

    let runTest = do
          port <- freePort

          let cfg = defaultConfig
                { cfgSaveFile = localFile
                , cfgReport = Just ("127.0.0.1", port)
                }

          server <- async $
            serveOn (Exactly 1) port serverFile

          let runClient = do
                -- Cho server khoi dong.
                threadDelay 500000

                world <- initSharedWorld
                  ((initGameState [])
                    { screen = GameOverScreen (Winner P1)
                    , matchTime = 25
                    , resultSent = False
                    })

                lock <- newMVar ()

                worker <- async (resultWorker cfg lock world)

                let checkResult = do
                      serverOutcome <- timeout 5000000 (waitCatch server)

                      case serverOutcome of
                        Nothing ->
                          expectationFailure
                            "TCP server did not receive the result"

                        Just (Left err) ->
                          expectationFailure
                            ("TCP server failed: " ++ show err)

                        Just (Right ()) ->
                          pure ()

                      saved <- loadSave serverFile

                      case saved of
                        Right sd ->
                          sdBoard sd `shouldBe` Scoreboard 1 0 0

                        Left err ->
                          expectationFailure
                            ("Server save error: " ++ show err)

                      final <- readWorld world
                      resultSent final `shouldBe` True

                checkResult `finally` cancel worker

          runClient `finally` cancel server

    outcome <- timeout 10000000 (runTest `finally` cleanup)

    case outcome of
      Nothing ->
        expectationFailure "TCP worker test timed out"

      Just () ->
        pure ()