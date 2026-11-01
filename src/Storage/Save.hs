-- Lưu/nạp bảng điểm và ảnh chụp trận đấu ra file văn bản (show/read)

module Storage.Save
  ( Scoreboard (..)
  , Snapshot (..)
  , SaveData (..)
  , emptySave
  , recordResult
  , snapshotOf
  , encodeSave
  , decodeSave
  , loadSave
  , writeSave
  , modifySave
  ) where

import Text.Read   (readMaybe)

import Model.Error
import Model.Types
import Model.Hp (hpValue)
import Storage.Files
import Control.Exception (IOException, try)
import System.Directory (doesPathExist, renameFile)
import System.IO (hPutStrLn, stderr)

-- Bảng điểm. Là Monoid nên có thể cộng gộp bảng điểm từ nhiều nguồn
data Scoreboard = Scoreboard
  { sbP1Wins :: !Int
  , sbP2Wins :: !Int
  , sbDraws  :: !Int
  } deriving (Show, Read, Eq)

instance Semigroup Scoreboard where
  Scoreboard a b c <> Scoreboard a' b' c' = Scoreboard (a + a') (b + b') (c + c')

instance Monoid Scoreboard where
  mempty = Scoreboard 0 0 0

-- Ảnh chụp trận đang chơi (luồng tự động lưu ghi định kỳ)
data Snapshot = Snapshot
  { snapMap     :: Int
  , snapSeconds :: Int
  , snapHp1     :: Int
  , snapHp2     :: Int
  } deriving (Show, Read, Eq)

data SaveData = SaveData
  { sdBoard :: Scoreboard
  , sdLast  :: Maybe Snapshot
  } deriving (Show, Read, Eq)

emptySave :: SaveData
emptySave = SaveData mempty Nothing

snapshotOf :: GameState -> Snapshot
snapshotOf gs =
  Snapshot
    (mapNumber (mapId gs))
    (matchTime gs)
    (hpValue (tankHp (tank1 gs)))
    (hpValue (tankHp (tank2 gs)))

-- Cộng một trận vào bảng điểm (hàm thuần
recordResult :: MatchResult -> SaveData -> SaveData
recordResult r sd = sd { sdBoard = sdBoard sd <> one }
  where
    one = case mrWinner r of
      Winner P1 -> mempty { sbP1Wins = 1 }
      Winner P2 -> mempty { sbP2Wins = 1 }
      Draw      -> mempty { sbDraws  = 1 }

encodeSave :: SaveData -> String
encodeSave = show

decodeSave :: FilePath -> String -> Either GameError SaveData
decodeSave path s = maybe (Left (CorruptSave path "cannot parse content")) Right (readMaybe s)

loadSave :: FilePath -> IO (Either GameError SaveData)
loadSave path = do
  r <- readFileStrict path
  pure (r >>= decodeSave path)

writeSave :: FilePath -> SaveData -> IO (Either GameError ())
writeSave path sd = writeFileSafe path (encodeSave sd ++ "\n")

-- sao lưu file save hỏng sang file .bak, tránh ghi đè mất dữ liệu
backupCorruptSave :: FilePath -> IO (Either GameError FilePath)
backupCorruptSave path = do
  backupPath <- findBackupPath 1
  result <- try (renameFile path backupPath)
            :: IO (Either IOException ())

  pure $ case result of
    Left err -> Left (CorruptSave path ("backup failed: " ++ show err))
    Right () -> Right backupPath
  where
    findBackupPath :: Int -> IO FilePath
    findBackupPath n = do
      let candidate =
            if n == 1
              then path ++ ".bak"
              else path ++ ".bak" ++ show n

      exists <- doesPathExist candidate

      if exists
        then findBackupPath (n + 1)
        else pure candidate

-- đọc file save, sửa đổi rồi ghi lại, tạo mới nếu chưa có, sao lưu nếu hỏng
modifySave :: FilePath -> (SaveData -> SaveData) -> IO (Either GameError ())
modifySave path f = do
  result <- loadSave path

  case result of
    Right sd ->
      writeSave path (f sd)

    Left (FileMissing _) ->
      writeSave path (f emptySave)

    Left (CorruptSave _ _) -> do
      backupResult <- backupCorruptSave path

      case backupResult of
        Left err ->
          pure (Left err)

        Right bak -> do
          hPutStrLn stderr ("[save] corrupt save moved to " ++ bak)
          writeSave path (f emptySave)

    Left err ->
      pure (Left err)
