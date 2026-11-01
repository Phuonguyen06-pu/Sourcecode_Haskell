module NetworkSpec (spec) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (finally)
import System.Directory (doesFileExist, removeFile)
import Test.Hspec

import Model.Types
import Net.Client
import Net.Server
import Parser.Protocol
import Storage.Save
import Support (freePort)

spec :: Spec
spec = describe "TCP Client - Server" $ do

  it "Gui RESULT va STATS qua socket that" $ do
    port <- freePort
    let file = "network-test-save.tmp"

    cleanup file

    done <- newEmptyMVar

    _ <- forkIO $
      serveOn (Exactly 2) port file
        `finally` putMVar done ()

    threadDelay 500000

    result <- sendCommand "127.0.0.1" port
                (Report (MatchResult Map1 (Winner P1) 30))

    result `shouldBe`
      Right "OK P1=1 P2=0 DRAW=0"

    stats <- sendCommand "127.0.0.1" port QueryStats

    stats `shouldBe`
      Right "P1=1 P2=0 DRAW=0"

    -- Cho server xu ly xong hai ket noi.
    takeMVar done

    saved <- loadSave file

    fmap sdBoard saved `shouldBe`
      Right (Scoreboard 1 0 0)

    cleanup file

cleanup :: FilePath -> IO ()
cleanup file = do
  exists <- doesFileExist file
  if exists
    then removeFile file
    else pure ()