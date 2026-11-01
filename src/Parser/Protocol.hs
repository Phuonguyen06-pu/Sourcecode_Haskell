-- Giao thức văn bản giữa client (game) và server
--
--   RESULT <map> <P1|P2|DRAW> <giây>   báo kết quả một trận
--   STATS                              hỏi bảng điểm tổng của server
--   PING                               kiem tra server con hoat dong
module Parser.Protocol
  ( Command (..)
  , parseCommand
  , encodeCommand
  ) where

import Data.Bifunctor (first)
import Data.Void      (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Model.Error
import Model.Types

type Parser = Parsec Void String

data Command
  = Report MatchResult
  | QueryStats
  | Ping
  deriving (Show, Eq)

winnerP :: Parser Winner
winnerP = choice
  [ Winner P1 <$ string "P1"
  , Winner P2 <$ string "P2"
  , Draw      <$ string "DRAW"
  ] <?> "winner (P1, P2 or DRAW)"

reportP :: Parser Command
reportP = do
  _ <- string "RESULT"
  hspace1
  n <- L.decimal
  m <- case mapFromNumber n of
    Just validMap -> pure validMap
    Nothing       -> fail "Invalid map ID (must be 1, 2 or 3)"
  hspace1
  w <- winnerP
  hspace1
  s <- L.decimal
  pure (Report (MatchResult m w s))

commandP :: Parser Command
commandP = hspace *> choice
  [ reportP
  , QueryStats <$ string "STATS"
  , Ping <$ string "PING"
  ] <* hspace <* eof

parseCommand :: String -> Either GameError Command
parseCommand = first (ProtocolError . errorBundlePretty) . parse commandP "<network>"

encodeCommand :: Command -> String
encodeCommand QueryStats = "STATS"
encodeCommand Ping = "PING"
encodeCommand (Report r) =
  unwords ["RESULT", show (mapNumber (mrMap r)), winnerText (mrWinner r), show (mrSeconds r)]
  where
    winnerText (Winner P1) = "P1"
    winnerText (Winner P2) = "P2"
    winnerText Draw        = "DRAW"
