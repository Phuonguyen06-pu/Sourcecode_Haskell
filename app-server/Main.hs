-- Server nhận kết quả trận qua TCP.
-- stack exec tank-battle-server -- [cổng] [file điểm]     (mặc định 9000, server-scores.txt)
module Main (main) where

import System.Environment (getArgs)

import Net.Server (ConnLimit(..), serveOn)

main :: IO ()
main = do
  args <- getArgs
  let port = case args of { (p : _) -> p; [] -> "9000" }
      file = case args of { (_ : f : _) -> f; _ -> "server-scores.txt" }
  serveOn Unlimited port file
