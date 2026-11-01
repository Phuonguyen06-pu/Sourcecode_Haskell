module Main (main) where

import Data.Maybe                       (listToMaybe)
import GHC.Clock                        (getMonotonicTimeNSec)
import Graphics.Gloss                   (Display (InWindow), makeColorI)
import Graphics.Gloss.Interface.IO.Game (Event (..), playIO)
import System.Environment               (getArgs)
import Text.Read                        (readMaybe)

import Bench.Benchmark   (runBenchmark)
import Bench.LazyDemo    (runLazyDemo)
import Concurrency.StateSync
import Concurrency.Workers (withWorkers)
import Engine.Input      (handleEvent)
import Engine.Step       (stepFrame)
import Levels.Maps       (loadMapTableWithErrors)
import Model.Config
import Model.Error       (describeError)
import Model.Types       (Seed (..), rngSeed, notice)
import Model.World       (initGameState)
import Net.Client        (sendCommand)
import Parser.ConfigParser (loadConfig)
import Parser.Protocol   (Command (QueryStats))
import UI.Render         (render)
import UI.Events (fromGlossEvent)

-- Chay game:        stack exec tank-battle-exe
-- Benchmark:        stack exec tank-battle-exe -- --bench 300 +RTS -N4 -RTS
-- Benchmark chunks: stack exec tank-battle-exe -- --bench-chunks 300 +RTS -N4 -RTS
-- Demo lazy/strict: stack exec tank-battle-exe -- --lazy-demo

main :: IO ()
main = do
  args <- getArgs
  case args of
    ("--bench" : rest) ->
      runBenchmark (maybe 300 id (readMaybe =<< listToMaybe rest)) False

    ("--bench-chunks" : rest) ->
      runBenchmark (maybe 300 id (readMaybe =<< listToMaybe rest)) True

    ("--lazy-demo" : _) ->
      runLazyDemo

    ("--stats" : host : port : _) ->
      sendCommand host port QueryStats
        >>= putStrLn . either describeError id

    _ ->
      runGame
      

-- Với -threaded, luồng main là bound thread; playIO chạy vòng lặp Gloss 60 FPS trên luồng này
-- Trạng thái game nằm trong TVar (STM); mọi thay đổi đi qua mutateWorld (atomically)
-- Ba luồng nền (đồng hồ, tự động lưu, ghi kết quả) chạy song song nhờ withWorkers
-- RNG bản đồ thuần túy: chỉ lấy seed đầu từ đồng hồ (IO), các trận sau tự sinh trong GameState (Levels.Arena.buildArena)
runGame :: IO ()
runGame = do
  cfg <- loadConfig "config.txt"
  (table, mapErrors) <- loadMapTableWithErrors "maps"
  t <- getMonotonicTimeNSec

  let start = (initGameState [])
        { rngSeed = Seed (fromIntegral t)
        , notice = case mapErrors of
            [] -> Nothing
            [err] -> Just err
            err : _ ->
              Just (show (length mapErrors) ++ " map errors. First: " ++ err)
        }
  world <- initSharedWorld start
  withWorkers cfg world $
    playIO
      (InWindow "Tank Battle: Parallel Combat" (windowWidth, windowHeight) (100, 50))
      (makeColorI 8 12 20 255)
      fps
      world
      (\w -> render table <$> readWorld w)
      (\ev w -> mutateWorld w (handleEvent table (fromGlossEvent (adjustEvent ev))) >> pure w)
      (\_ w -> mutateWorld w stepFrame >> pure w)


adjustEvent :: Event -> Event
adjustEvent (EventKey k ks mods (x, y)) =
  EventKey k ks mods (x / viewScale, y / viewScale)
adjustEvent ev = ev
