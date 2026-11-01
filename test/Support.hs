module Support (freePort, bracketSocket) where

import Control.Exception (bracket)
import Network.Socket

-- | Lay mot cong TCP con trong do he dieu hanh cap.
freePort :: IO String
freePort = withSocketsDo $
  bracketSocket $ \sock -> do
    bind sock (SockAddrInet 0 (tupleToHostAddress (127, 0, 0, 1)))

    addr <- getSocketName sock

    case addr of
      SockAddrInet port _ -> pure (show port)
      _ -> fail "Cannot allocate TCP port"

-- | Tao socket va dam bao socket duoc dong sau khi su dung.
bracketSocket :: (Socket -> IO a) -> IO a
bracketSocket =
  bracket
    (socket AF_INET Stream defaultProtocol)
    close