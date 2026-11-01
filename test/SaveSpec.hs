-- | Kiểm thử lưu/nạp: phần thuần bằng QuickCheck, phần IO bằng file tạm.
module SaveSpec (spec) where

import System.Directory (doesFileExist, removeFile)
import Control.Monad (forM_)
import Test.Hspec
import Test.QuickCheck

import Model.Error
import Model.Types
import Storage.Save

genBoard :: Gen Scoreboard
genBoard = Scoreboard <$> choose (0, 500) <*> choose (0, 500) <*> choose (0, 500)

genSnapshot :: Gen Snapshot
genSnapshot = Snapshot <$> choose (1, 3) <*> choose (0, 9999) <*> choose (0, 100) <*> choose (0, 100)

genSave :: Gen SaveData
genSave = SaveData <$> genBoard <*> oneof [pure Nothing, Just <$> genSnapshot]

genResult :: Gen MatchResult
genResult = MatchResult <$> elements [Map1, Map2, Map3] <*> elements [Winner P1, Winner P2, Draw] <*> choose (0, 999)
totalGames :: Scoreboard -> Int
totalGames b = sbP1Wins b + sbP2Wins b + sbDraws b

tmpFile :: FilePath
tmpFile = "tank-battle-test-save.tmp"

cleanup :: IO ()
cleanup = do
  let files = tmpFile : [tmpFile ++ ".bak" ++ suffix | suffix <- "" : map show [2..100]]

  forM_ files $ \file -> do
    exists <- doesFileExist file
    if exists
      then removeFile file
      else pure ()

spec :: Spec
spec = describe "Save / load" $ do

  it "load (save x) == x (QuickCheck, 200 mau)" $
    property $ withMaxSuccess 200 $ forAll genSave $ \sd ->
      decodeSave "f" (encodeSave sd) == Right sd

  it "recordResult tang tong so tran dung 1" $
    property $ forAll ((,) <$> genSave <*> genResult) $ \(sd, r) ->
      totalGames (sdBoard (recordResult r sd)) == totalGames (sdBoard sd) + 1

  it "Scoreboard la Monoid: ket hop co tinh ket hop" $
    property $ forAll ((,,) <$> genBoard <*> genBoard <*> genBoard) $ \(a, b, c) ->
      (a <> b) <> c == a <> (b <> c) && mempty <> a == a

  it "noi dung hong => CorruptSave, khong nem ngoai le" $
    decodeSave "f" "day khong phai du lieu" `shouldSatisfy` isCorrupt

  it "ghi roi doc lai tu file that" $ do
    cleanup
    r1 <- writeSave tmpFile (SaveData (Scoreboard 1 2 3) Nothing)
    r1 `shouldBe` Right ()
    loadSave tmpFile `shouldReturn` Right (SaveData (Scoreboard 1 2 3) Nothing)
    cleanup

  it "doc file khong ton tai => FileMissing" $ do
    cleanup
    r <- loadSave tmpFile
    r `shouldBe` Left (FileMissing tmpFile)

  it "modifySave tao file moi roi cong don qua nhieu lan" $ do
    cleanup
    _ <- modifySave tmpFile (recordResult (MatchResult Map1 (Winner P1) 5))
    _ <- modifySave tmpFile (recordResult (MatchResult Map1 (Winner P1) 7))
    _ <- modifySave tmpFile (recordResult (MatchResult Map2 Draw 9))
    r <- loadSave tmpFile
    fmap sdBoard r `shouldBe` Right (Scoreboard 2 0 1)
    cleanup

  it "file hong duoc sao luu thanh .bak va tao file moi" $ do
    cleanup
    writeFile tmpFile "rac"

    r <- modifySave tmpFile (recordResult (MatchResult Map1 (Winner P2) 5))

    r `shouldBe` Right ()

    -- File moi phai chua du lieu hop le
    result <- loadSave tmpFile
    case result of
      Right sd -> do
        sbP1Wins (sdBoard sd) `shouldBe` 0
        sbP2Wins (sdBoard sd) `shouldBe` 1
        sbDraws  (sdBoard sd) `shouldBe` 0
      Left err ->
        expectationFailure ("Khong doc duoc file moi: " ++ show err)

    -- File hong ban dau phai duoc bao toan
    backupContent <- readFile (tmpFile ++ ".bak")
    backupContent `shouldBe` "rac"

    cleanup

isCorrupt :: Either GameError a -> Bool
isCorrupt (Left (CorruptSave _ _)) = True
isCorrupt _                        = False
