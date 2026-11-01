-- nhận dữ liệu từ socket và tách dữ liệu thành từng dòng hoàn chỉnh

module Net.Line (receiveLine, scanBuffer) where

import Network.Socket (Socket)
import Network.Socket.ByteString (recv)
import qualified Data.ByteString.Char8 as BS

import Model.Config (maxLineBytes)
import Model.Error (GameError(..))

stripCR :: String -> String
stripCR = reverse . dropWhile (== '\r') . reverse

-- kiểm tra xem buffer có chứa một dòng dữ liệu hợp lệ không, trả về lỗi nếu quá dài
scanBuffer :: Int -> BS.ByteString -> Either GameError (Maybe String)
scanBuffer limit buf
  | BS.length line > limit = Left (LineTooLong limit)
  | BS.null rest           = Right Nothing
  | otherwise              = Right (Just (stripCR (BS.unpack line)))
  where
    (line, rest) = BS.break (== '\n') buf

-- nhận dòng dữ liệu từ socket
receiveLine :: Socket -> IO (Either GameError String)
receiveLine sock = go BS.empty
  where
    go acc = do
      chunk <- recv sock 4096

      if BS.null chunk
        then pure (Left (NetworkError "connection closed before newline"))
        else do
          let combined = BS.append acc chunk

          case scanBuffer maxLineBytes combined of
            Left err          -> pure (Left err)
            Right (Just line) -> pure (Right line)
            Right Nothing     -> go combined