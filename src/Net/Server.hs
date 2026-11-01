-- Server TCP đơn giản nhận kết quả trận từ các client (game)
--
-- Mỗi kết nối chạy trong một luồng riêng (forkIO). Bảng điểm chung nằm trong
-- một TVar nên nhiều client cập nhật cùng lúc vẫn nhất quán (STM). File lưu
-- được bảo vệ bằng MVar để hai luồng không ghi chồng lên nhau
module Net.Server (ConnLimit(..), serveOn, handleCommand) where

import Control.Concurrent      (forkIO)
import Control.Concurrent.MVar
  (MVar, newMVar, newEmptyMVar, putMVar, takeMVar, withMVar)
import Control.Concurrent.STM  (TVar, atomically, newTVarIO, readTVar, writeTVar)
import Control.Exception       (IOException, bracket, finally, try)
import System.Timeout          (timeout)
import Control.Monad (forever, void, replicateM_)
import Network.Socket
import System.IO
import qualified Data.ByteString.Char8 as BS
import Network.Socket.ByteString (sendAll)
import Net.Line (receiveLine)

import Model.Config (serverTimeoutUs, serverListenBacklog)
import Model.Error
import Parser.Protocol
import Storage.Save

-- Phần thuần của server: lệnh + bảng điểm cũ => (câu trả lời, bảng điểm mới)
handleCommand :: Command -> Scoreboard -> (String, Scoreboard)
handleCommand Ping board = ("PONG", board)
handleCommand QueryStats board = (summary board, board)
handleCommand (Report r) board = ("OK " ++ summary board', board')
  where board' = sdBoard (recordResult r emptySave { sdBoard = board })

-- Giới hạn số kết nối đồng thời (không giới hạn hoặc đúng N kết nối)
data ConnLimit
  = Unlimited
  | Exactly Int
  deriving (Show, Eq)

summary :: Scoreboard -> String
summary b = "P1=" ++ show (sbP1Wins b) ++ " P2=" ++ show (sbP2Wins b) ++ " DRAW=" ++ show (sbDraws b)

-- Khởi tạo bảng điểm từ file lưu (nếu có) hoặc tạo mới nếu file không tồn tại.
-- Nếu file bị hỏng thì sao lưu thành .bak trước khi tạo mới
initScoreboard :: FilePath -> IO Scoreboard
initScoreboard file = do
  saved <- loadSave file

  case saved of
    Right sd ->
      pure (sdBoard sd)

    Left (FileMissing _) ->
      pure mempty

    Left (CorruptSave _ _) -> do
      result <- modifySave file id

      case result of
        Right () -> do
          putStrLn "[server] corrupt save backed up, starting fresh"
          pure mempty

        Left err ->
          ioError (userError
            ("[server] cannot recover save: " ++ describeError err))

    Left err ->
      ioError (userError
        ("[server] cannot load save: " ++ describeError err))

-- chạy TCP với số lượng kết nối đồng thời giới hạn, lắng nghe trên cổng port và dùng file lưu file
serveOn :: ConnLimit -> String -> FilePath -> IO ()
serveOn limit port file = withSocketsDo $ do
  board0 <- initScoreboard file

-- TVar: lưu bảng điểm dùng chung cho các client
  tv   <- newTVarIO board0

-- MVar: khóa bao ve quá trình cập nhật và ghi file.
-- Đảm bảo hai client không ghi file cùng lúc.
  lock <- newMVar ()

  let hints = defaultHints
        { addrFlags = [AI_PASSIVE]
        , addrSocketType = Stream
        }

  addrs <- getAddrInfo (Just hints) Nothing (Just port)

  case addrs of
    [] -> fail "[server] cannot resolve listening address"

    (addr : _) ->
      bracket (openSocket addr) close $ \sock -> do
        setSocketOption sock ReuseAddr 1
        bind sock (addrAddress addr)
        listen sock serverListenBacklog

        putStrLn
          ("[server] listening on port " ++ port
            ++ ", scores file " ++ file)

        case limit of
          Unlimited ->
            forever $ do
              (conn, _) <- accept sock
              void (forkIO (serveClient tv lock file conn))

          Exactly n -> do
            done <- newEmptyMVar

            let loop 0 = pure ()
                loop k = do
                  (conn, _) <- accept sock

                  void $ forkIO $
                    serveClient tv lock file conn
                      `finally` putMVar done ()

                  loop (k - 1)

            loop (max 0 n)

            replicateM_ (max 0 n) (takeMVar done)

serveClient :: TVar Scoreboard -> MVar () -> FilePath -> Socket -> IO ()
serveClient tv lock file conn = do
  result <- try (talk `finally` close conn)
            :: IO (Either IOException ())

  case result of
    Left err ->
      hPutStrLn stderr ("[server] client error: " ++ show err)

    Right () ->
      pure ()

  where
    talk = do
      input <- timeout serverTimeoutUs (receiveLine conn)

      case input of
        Nothing ->
          hPutStrLn stderr "[server] client timeout"

        Just (Left err) ->
          sendReply ("ERROR " ++ map flat (describeError err))

        Just (Right line) -> do
          reply <- case parseCommand line of

            Left err ->
              pure ("ERROR " ++ map flat (describeError err))

            Right QueryStats -> do
              board <- atomically (readTVar tv)
              pure (fst (handleCommand QueryStats board))

            Right Ping -> do
              board <- atomically (readTVar tv)
              pure (fst (handleCommand Ping board))

            Right cmd@(Report _) ->
              -- giữ khóa trong suốt quá trình đọc bảng điểm
              -- ghi file và cập nhật TVar để tránh mất kết quả.
              withMVar lock $ \_ -> do
                old <- atomically (readTVar tv)

                let (msg, newBoard) = handleCommand cmd old

                result <- modifySave file
                  (\sd -> sd { sdBoard = newBoard })

                case result of
                  Left err -> do
                    hPutStrLn stderr
                      ("[server] save error: " ++ describeError err)

                    pure "ERROR cannot save result"

                  Right () -> do
                    atomically (writeTVar tv newBoard)

                    putStrLn
                      ("[server] " ++ line ++ " -> " ++ msg)

                    pure msg

          sendReply reply

    sendReply reply =
      sendAll conn (BS.pack (reply ++ "\n"))

    flat c =
      if c == '\n' || c == '\r'
        then ' '
        else c

