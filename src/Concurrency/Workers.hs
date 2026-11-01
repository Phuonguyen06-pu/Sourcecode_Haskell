-- Các tác vụ nền chạy ĐỒNG THỜI với vòng lặp Gloss:
--
--   clockWorker    : mỗi giây tăng matchTime trong TVar (STM)
--   autosaveWorker : mỗi N giây đọc TVar và ghi ảnh chụp trận ra file
--   resultWorker   : chờ trận kết thúc, ghi kết quả ra file và gửi lên server
--
-- Cơ chế đồng bộ:
--   TVar + STM: đồng bộ trạng thái game giữa các luồng, tránh race condition và deadlock
--   MVar (saveLock): autosaveWorker và resultWorker cùng ghi file, dùng MVar để tránh ghi đè lẫn nhau
-- 'withAsync' đảm bảo các luồng nền bị hủy khi cửa sổ game đóng

module Concurrency.Workers
  ( withWorkers
  , supervised
  , autosaveWorker
  , resultWorker
  ) where

import Control.Concurrent       (threadDelay)
import Control.Exception
  ( SomeException
  , SomeAsyncException
  , try
  , fromException
  , throwIO
  )
import Control.Concurrent.Async (withAsync)
import Control.Concurrent.MVar  (MVar, newMVar, withMVar)
import Control.Concurrent.STM   (atomically)
import Control.Monad            (forever, when)
import System.IO                (hPutStrLn, stderr)
import Data.List (isPrefixOf)

import Concurrency.StateSync
import Model.Describable
import Model.Error
import Model.Types
import Net.Client           (sendCommand)
import Parser.ConfigParser  (RuntimeConfig (..))
import Parser.Protocol      (Command (..))
import Storage.Save

-- Chạy phần thân (vòng lặp game) cùng ba luồng nền, hủy khi phần thân kết thúc
withWorkers :: RuntimeConfig -> SharedWorld -> IO a -> IO a
withWorkers cfg world body = do
  -- MVar dùng làm khóa chung cho hai worker ghi file
  -- Đảm bảo autosave và lưu kết quả không ghi đè lên nhau
  lock <- newMVar ()
  withAsync (supervised "clock" (clockWorker world)) $ \_ ->
    withAsync (supervised "autosave" (autosaveWorker cfg lock world)) $ \_ ->
      withAsync (supervised "result" (resultWorker cfg lock world)) $ \_ ->
        body

clockWorker :: SharedWorld -> IO ()
clockWorker world = forever $ do
  threadDelay 1000000

  -- Cập nhật thời gian
  mutateWorld world tickClock

autosaveWorker :: RuntimeConfig -> MVar () -> SharedWorld -> IO ()
autosaveWorker cfg lock world = forever $ do
  threadDelay (cfgAutosaveSecs cfg * 1000000)
  gs <- readWorld world
  when (screen gs == PlayScreen) $ do
    -- Lấy khóa MVar trước khi ghi snapshot xuống file
    -- Khóa được giải phóng sau khi modifySave hoàn thành
    r <- withMVar lock $ \_ ->
      modifySave (cfgSaveFile cfg)
        (\sd -> sd { sdLast = Just (snapshotOf gs) })
    reportError r
    putStrLn (logLine "autosave" (screen gs) ++ " t=" ++ show (matchTime gs) ++ "s | P1 "
              ++ describe (tank1 gs) ++ " | P2 " ++ describe (tank2 gs))

resultWorker :: RuntimeConfig -> MVar () -> SharedWorld -> IO ()
resultWorker cfg lock world = forever $ do
  -- STM retry cho worker cho đến khi có kết quả trận đấu
  -- không cần liên tục kiểm tra trạng thái bằng vòng lặp polling
  result <- atomically (awaitResult world)
  -- Dùng MVar với autosave để tránh ghi file đồng thời
  r <- withMVar lock $ \_ ->
    modifySave (cfgSaveFile cfg) (recordResult result)
  reportError r
  putStrLn (logLine "result" (mrWinner result) ++ " on map " ++ show (mapNumber (mrMap result))
            ++ " after " ++ show (mrSeconds result) ++ "s")
  case cfgReport cfg of
    Nothing          -> pure ()
    Just (host, port) -> do
      reply <- sendCommand host port (Report result)
      case reply of
        Left err -> do
          hPutStrLn stderr ("[net] " ++ describeError err)

          mutateWorld world $ \gs ->
            gs { notice = Just ("Server: " ++ describeError err) }

        Right msg
          | "ERROR" `isPrefixOf` msg -> do
              hPutStrLn stderr ("[net] server error: " ++ msg)

              mutateWorld world $ \gs ->
                gs { notice = Just ("Server: " ++ msg) }

          | otherwise ->
              putStrLn ("[net] server replied: " ++ msg)

reportError :: Either GameError () -> IO ()
reportError (Left err) = hPutStrLn stderr ("[save] " ++ describeError err)
reportError (Right ()) = pure ()

--  giám sát worker
--   lỗi đồng bộ: ghi log và thử lại sau 1 giây
--   lỗi bất đồng bộ: ném lại để worker có thể bị hủy
supervised :: String -> IO () -> IO ()
supervised name action = loop
  where
    loop = do
      result <- try action :: IO (Either SomeException ())

      case result of
        Right () -> pure ()

        Left err ->
          case fromException err :: Maybe SomeAsyncException of
            Just _ ->
              throwIO err

            Nothing -> do
              hPutStrLn stderr
                ("[worker:" ++ name ++ "] " ++ show err)

              threadDelay 1000000
              loop