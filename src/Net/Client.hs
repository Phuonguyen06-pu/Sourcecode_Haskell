-- Client TCP: gửi một lệnh tới server rồi đọc một dòng trả lời
-- Mọi lỗi mạng (không kết nối được, quá thời gian, server đóng) trả về
-- @Left NetworkError@, không làm game dừng
module Net.Client (sendCommand) where

import Control.Exception (IOException, bracket, bracketOnError, try)
import Network.Socket
import System.Timeout    (timeout)
import qualified Data.ByteString.Char8 as BS
import Network.Socket.ByteString (sendAll)
import Net.Line (receiveLine)
import Model.Config (clientTimeoutUs)

import Model.Error
import Parser.Protocol

sendCommand :: String -> String -> Command -> IO (Either GameError String)
sendCommand host port cmd = do
  r <- try (timeout clientTimeoutUs (withSocketsDo go))
       :: IO (Either IOException (Maybe (Either GameError String)))

  pure $ case r of
    Left e                   -> Left (NetworkError (show e))
    Right Nothing            -> Left (NetworkError "timeout")
    Right (Just (Left err))   -> Left err
    Right (Just (Right msg))  -> Right msg

  where
    go = do
      addrs <- getAddrInfo
        (Just (defaultHints { addrSocketType = Stream }))
        (Just host)
        (Just port)

      case addrs of
        [] -> ioError (userError "cannot resolve host")

        (addr : _) ->
          bracket (connectTo addr) close $ \sock -> do
            sendAll sock (BS.pack (encodeCommand cmd ++ "\n"))
            receiveLine sock

    connectTo addr =
      bracketOnError (openSocket addr) close $ \sock -> do
        connect sock (addrAddress addr)
        pure sock

