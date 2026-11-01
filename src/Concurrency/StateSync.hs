-- Đồng bộ trạng thái chia sẻ bằng STM
--
-- GameState nằm trong một TVar và có nhiều luồng cùng truy cập:
--   luồng Gloss (60 lần/giây): xử lý phím và 'stepFrame'
--   luồng đồng hồ: mỗi giây tăng 'matchTime'
--   luồng tự động lưu: đọc định kỳ
--   luồng ghi kết quả: chờ trận kết thúc bằng 'awaitResult'

module Concurrency.StateSync
  ( SharedWorld
  , initSharedWorld
  , readWorld
  , mutateWorld
  , tickClock
  , awaitResult
  ) where

import Control.Concurrent.STM (STM, TVar, atomically, modifyTVar', newTVarIO, readTVar, retry, writeTVar)
import Control.DeepSeq        (force)

import Model.Types

type SharedWorld = TVar GameState

initSharedWorld :: GameState -> IO SharedWorld
initSharedWorld = newTVarIO

readWorld :: SharedWorld -> IO GameState
readWorld = atomically . readTVar

-- Cập nhật trạng thái game an toàn bằng STM
-- force đánh giá sau, tránh tích tụ thunk
mutateWorld :: SharedWorld -> (GameState -> GameState) -> IO ()
mutateWorld tv f = atomically (modifyTVar' tv (force . f))

-- chỉ đếm giờ khi đang chơi
tickClock :: GameState -> GameState
tickClock gs
  | screen gs == PlayScreen = gs { matchTime = matchTime gs + 1 }
  | otherwise               = gs

-- Chờ trận kết thúc và trả về kết quả
-- đánh dâu kết quả đã gửi để tránh xử lý một trận nhiều lần
awaitResult :: SharedWorld -> STM MatchResult
awaitResult tv = do
  gs <- readTVar tv
  case screen gs of
    GameOverScreen w | not (resultSent gs) -> do
      writeTVar tv gs { resultSent = True }
      pure (MatchResult (mapId gs) w (matchTime gs))
    _ -> retry
