-- ĐÁNH GIÁ LƯỜI và ĐÁNH GIÁ CHẶT 
--  1. Lười: @iterate@ sinh một danh sách VÔ HẠN các trạng thá,  @findIndex@ chỉ
--     đánh giá đến khung hình kết thúc trận, phần còn lại không bao giờ được tính
--  2. Lười: phần tử lỗi trong danh sách không làm hỏng chương trình nếu không ai đọc nó
--  3. Chặt: @force@ ép đánh giá sâu nên phần tử lỗi bị phát hiện ngay

module Bench.LazyDemo (runLazyDemo) where

import Control.DeepSeq   (force)
import Control.Exception (ErrorCall (..), evaluate, try)
import Data.List         (findIndex)

import Engine.Input  (tryFire)
import Engine.Step   (stepFrame)
import Model.Types
import Model.World   (initGameState)

isOver :: GameState -> Bool
isOver gs = case screen gs of
  GameOverScreen _ -> True
  _                -> False

start :: GameState
start = (initGameState [])
  { screen = PlayScreen
  , tank1 = mkTank (Vec2 (-150) 0) 0 100
  , tank2 = mkTank (Vec2 150 0) 180 100
  }

-- Mô phỏng trận đấu
-- Mỗi khung hình, P1 luôn bắn ra một viên đạn (P2 không bắn). sau đó game cập nhật trạng thái
-- nếu trận đấu kết thúc, giữ nguyên trạng thái hiện tại
scripted :: GameState -> GameState
scripted gs
  | isOver gs = gs
  | otherwise = stepFrame (tryFire P1 gs)

runLazyDemo :: IO ()
runLazyDemo = do
  putStrLn "== 1. Lazy: infinite list of game states =="
  let timeline = iterate scripted start      -- danh sách vô hạn
  case findIndex isOver (take 20000 timeline) of
    Nothing -> putStrLn "no game over within 20000 frames"
    Just n  -> do
      putStrLn ("game over at frame " ++ show n ++ " (only these frames were evaluated)")
      putStrLn ("result: " ++ show (screen (timeline !! n)))

  putStrLn "== 2. Lazy: an unused bad element is never evaluated =="
  let xs = [scripted start, error "boom"] :: [GameState]
  putStrLn ("length = " ++ show (length xs) ++ " (no exception)")

  putStrLn "== 3. Strict: force evaluates everything immediately =="
  r <- try (evaluate (force xs)) :: IO (Either ErrorCall [GameState])
  case r of
    Left (ErrorCall msg) -> putStrLn ("force raised: " ++ msg)
    Right _              -> putStrLn "no exception (unexpected)"
