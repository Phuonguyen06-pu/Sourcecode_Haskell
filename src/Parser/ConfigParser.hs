-- Đọc file cấu hình dạng khóa = giá trị
--
--   autosave  = 5               giây giữa hai lần tự động lưu
--   save_file = savegame.txt    file lưu bảng điểm
--   report    = localhost:9000  địa chỉ server nhận kết quả trận (tùy chọn)

module Parser.ConfigParser
  ( RuntimeConfig (..)
  , defaultConfig
  , parseConfigText
  , loadConfig
  ) where

import Data.Bifunctor (first)
import Data.Char      (isSpace)
import Data.List      (dropWhileEnd)
import Data.Maybe     (catMaybes)
import Data.Void      (Void)
import System.IO      (hPutStrLn, stderr)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Model.Error
import Storage.Files (readFileStrict)

type Parser = Parsec Void String

data RuntimeConfig = RuntimeConfig
  { cfgAutosaveSecs :: Int
  , cfgSaveFile     :: FilePath
  , cfgReport       :: Maybe (String, String)   -- (host, port)
  } deriving (Show, Eq)

defaultConfig :: RuntimeConfig
defaultConfig = RuntimeConfig
  { cfgAutosaveSecs = 5
  , cfgSaveFile     = "savegame.txt"
  , cfgReport       = Nothing
  }

data Setting
  = Autosave Int
  | SaveFile FilePath
  | ReportTo String String

settingP :: Parser Setting
settingP = choice
  [ Autosave <$> (key "autosave" *> L.decimal)
  , SaveFile <$> (key "save_file" *> fileValue)
  , reportP
  ]

key :: String -> Parser ()
key name = string name *> hspace *> char '=' *> hspace *> pure ()
  <?> ("setting '" ++ name ++ "'")

fileValue :: Parser String
fileValue = dropWhileEnd isSpace <$> takeWhile1P (Just "file name") (`notElem` "\r\n#;")

reportP :: Parser Setting
reportP = do
  key "report"
  h <- some (alphaNumChar <|> oneOf ".-_")
  _ <- char ':' <?> "':' between host and port"
  p <- some digitChar <?> "port number"
  pure (ReportTo h p)

commentP :: Parser ()
commentP = (char '#' <|> char ';') *> takeWhileP Nothing (/= '\n') *> pure ()

lineP :: Parser (Maybe Setting)
lineP = hspace *> ((Nothing <$ commentP) <|> (Just <$> settingP) <|> pure Nothing) <* hspace

configP :: Parser [Setting]
configP = catMaybes <$> (lineP `sepEndBy` eol) <* eof

apply :: RuntimeConfig -> Setting -> RuntimeConfig
apply c (Autosave n)    = c { cfgAutosaveSecs = n }
apply c (SaveFile f)    = c { cfgSaveFile = f }
apply c (ReportTo h p)  = c { cfgReport = Just (h, p) }

-- Phân tích rồi kiểm tra giá trị hợp lệ (autosave phải > 0).
parseConfigText :: FilePath -> String -> Either GameError RuntimeConfig
parseConfigText path src = do
  settings <- first (ConfigParseError path . errorBundlePretty) (parse configP path src)
  let cfg = foldl apply defaultConfig settings
  if cfgAutosaveSecs cfg <= 0
    then Left (ConfigParseError path "autosave must be greater than 0")
    else Right cfg

-- Thiếu file thì dùng mặc định, file sai thì báo lỗi rồi dùng mặc định
loadConfig :: FilePath -> IO RuntimeConfig
loadConfig path = do
  r <- readFileStrict path
  case r >>= parseConfigText path of
    Right cfg            -> pure cfg
    Left (FileMissing _) -> pure defaultConfig
    Left err             -> do
      hPutStrLn stderr ("[config] " ++ describeError err ++ " -> using defaults")
      pure defaultConfig
