--  Đọc/ghi file an toàn và chuyển lỗi IO thành 'GameError'

module Storage.Files (readFileStrict, writeFileSafe) where

import Control.Exception (IOException, evaluate, try, bracketOnError)
import System.Directory   (renameFile, removeFile)
import System.FilePath    (takeDirectory, takeFileName)
import System.IO          (hClose, hPutStr, openTempFile)
import System.IO.Error    (isDoesNotExistError)

import Model.Error

readFileStrict :: FilePath -> IO (Either GameError String)
readFileStrict path = do
  r <- try (readFile path >>= \s -> evaluate (length s) >> pure s)
       :: IO (Either IOException String)

  pure $ case r of
    Right s -> Right s
    Left e
      | isDoesNotExistError e -> Left (FileMissing path)
      | otherwise             -> Left (FileError path (show e))

-- ghi file an toàn: ghi vào file tạm rồi rename sang file đích, tránh ghi dở dang
writeFileSafe :: FilePath -> String -> IO (Either GameError ())
writeFileSafe path content = do
  result <- try writeAtomically :: IO (Either IOException ())

  pure $ case result of
    Right () -> Right ()
    Left err -> Left (FileError path (show err))

  where
    writeAtomically =
      bracketOnError
        (openTempFile (takeDirectory path) (takeFileName path ++ ".tmp"))
        cleanup
        (\(tempPath, handle) -> do
            hPutStr handle content
            hClose handle
            renameFile tempPath path
        )

    cleanup (tempPath, handle) = do
      _ <- try (hClose handle) :: IO (Either IOException ())
      _ <- try (removeFile tempPath) :: IO (Either IOException ())
      pure ()