-- | Kiểm thử ba parser Megaparsec: map, cấu hình, giao thức mạng.
module ParserSpec (spec) where

import Data.List (isInfixOf)
import Control.Monad (forM_)
import Test.Hspec
import Test.QuickCheck

import Engine.Collision (tankBlockedAt)
import Model.Config
import Model.Error
import Model.Types
import Model.World (spawnP1, spawnP2)
import Parser.ConfigParser
import Parser.MapParser
import Parser.Protocol
import Net.Server (handleCommand)
import Storage.Save (Scoreboard)

-- | Lưới 32 x 18 toàn ô trống.
blankRows :: [String]
blankRows = replicate arenaRows (replicate arenaCols '.')

-- | Đặt ký tự @ch@ vào ô (cột, hàng).
withCell :: Int -> Int -> Char -> [String] -> [String]
withCell col row ch rows =
  [ if r == row then take col l ++ [ch] ++ drop (col + 1) l else l
  | (r, l) <- zip [0 :: Int ..] rows ]

-- | Lấy thông điệp lỗi map (nếu có).
mapError :: Either GameError a -> Maybe String
mapError (Left (MapParseError _ m)) = Just m
mapError _                          = Nothing

spec :: Spec
spec = describe "Parsers" $ do

  describe "MapParser" $ do
    it "luoi 32x18 toan o trong la ban do hop le, khong co gi" $
      parseMapText "t" (unlines blankRows) `shouldBe` Right (MapLayout [] [] [])
    it "doc dung loai vat can va mau mac dinh cua tung loai" $ do
      let txt = unlines (withCell 2 0 'X' (withCell 1 0 'B' (withCell 0 0 '#' blankRows)))
      fmap (map wallType . layoutWalls) (parseMapText "t" txt)
        `shouldBe` Right [SolidSteel, BreakableBrick, ExplosiveBarrel]
      fmap (map wallHp . layoutWalls) (parseMapText "t" txt)
        `shouldBe` Right [steelHp, brickMaxHp, barrelHp]
    it "toa do: o goc tren-trai la (-465, 255), goc duoi-phai la (465, -255)" $ do
      let tl = unlines (withCell 0 0 '#' blankRows)
          br = unlines (withCell 31 17 '#' blankRows)
      fmap (map wallPos . layoutWalls) (parseMapText "t" tl) `shouldBe` Right [Vec2 (-465) 255]
      fmap (map wallPos . layoutWalls) (parseMapText "t" br) `shouldBe` Right [Vec2 465 (-255)]
    it "san dac biet: bun ~, bay T, lo xo S (khong chan duong)" $ do
      let txt = unlines (withCell 7 5 'S' (withCell 6 5 'T' (withCell 5 5 '~' blankRows)))
      fmap (map floorKind . layoutFloors) (parseMapText "t" txt) `shouldBe` Right [Mud, Trap, Spring]
      fmap layoutWalls (parseMapText "t" txt) `shouldBe` Right []
    it "o ngau nhien ? duoc ghi lai de quyet dinh moi tran" $
      fmap layoutRandom (parseMapText "t" (unlines (withCell 4 4 '?' blankRows)))
        `shouldBe` Right [cellPos 4 4]
    it "hai cong dich chuyen O duoc noi voi nhau" $ do
      let txt = unlines (withCell 28 14 'O' (withCell 3 3 'O' blankRows))
          a   = cellPos 3 3
          b   = cellPos 28 14
      fmap layoutFloors (parseMapText "t" txt)
        `shouldBe` Right [Floor a (Teleporter b), Floor b (Teleporter a)]
    it "chi co mot cong O => loi noi ro" $
      mapError (parseMapText "t" (unlines (withCell 3 3 'O' blankRows)))
        `shouldSatisfy` maybe False ("teleporter" `isInfixOf`)
    it "ba cong O => loi noi ro so luong" $
      mapError (parseMapText "t" (unlines (withCell 9 9 'O' (withCell 5 5 'O' (withCell 1 1 'O' blankRows)))))
        `shouldSatisfy` maybe False ("found 3" `isInfixOf`)
    it "bo qua chu thich va dong trong" $
      parseMapText "t" ("; chu thich\n\n" ++ unlines blankRows ++ "\n; het\n")
        `shouldBe` Right (MapLayout [] [] [])
    it "chap nhan xuong dong kieu Windows (CRLF)" $
      parseMapText "t" (concatMap (++ "\r\n") blankRows) `shouldBe` Right (MapLayout [] [] [])
    it "ky tu la => loi co dong/cot" $
      mapError (parseMapText "t.txt" (unlines (withCell 1 0 'Z' blankRows)))
        `shouldSatisfy` maybe False ("1:2" `isInfixOf`)
    it "cac dong khac do dai => loi chi ro dong sai" $
      mapError (parseMapText "t" (unlines (take 1 blankRows ++ [replicate 5 '.'] ++ drop 2 blankRows)))
        `shouldSatisfy` maybe False ("line 2" `isInfixOf`)
    it "luoi sai kich thuoc => loi neu kich thuoc can co" $
      mapError (parseMapText "t" "...\n...\n...\n")
        `shouldSatisfy` maybe False ("expected 32x18" `isInfixOf`)
    it "file rong => loi" $
      mapError (parseMapText "t" "") `shouldSatisfy` maybe False ("empty" `isInfixOf`)

  describe "Map files in maps/" $ do
    forM_ [1 .. 3 :: Int] $ \n ->
      it ("map" ++ show n ++ ".txt doc duoc, co vien thep va hai diem xuat phat thoang") $ do
        r <- loadMapFile ("maps/map" ++ show n ++ ".txt")
        case r of
          Left err  -> expectationFailure (show err)
          Right lay -> do
            layoutWalls lay `shouldSatisfy` (not . null)
            tankBlockedAt (layoutWalls lay) spawnP1 `shouldBe` False
            tankBlockedAt (layoutWalls lay) spawnP2 `shouldBe` False
    it "map1 chi co o ngau nhien, khong co san dac biet" $ do
      Right lay <- loadMapFile "maps/map1.txt"
      layoutFloors lay `shouldBe` []
      layoutRandom lay `shouldSatisfy` (not . null)
    it "map2: 2 cong dich chuyen, 4 bay dien, 36 o bun" $ do
      Right lay <- loadMapFile "maps/map2.txt"
      let kinds = map floorKind (layoutFloors lay)
      length [() | Teleporter _ <- kinds] `shouldBe` 2
      length (filter (== Trap) kinds) `shouldBe` 4
      length (filter (== Mud) kinds) `shouldBe` 36
    it "map3: 4 lo xo, 8 thung no" $ do
      Right lay <- loadMapFile "maps/map3.txt"
      length (filter ((== Spring) . floorKind) (layoutFloors lay)) `shouldBe` 4
      length (filter ((== ExplosiveBarrel) . wallType) (layoutWalls lay)) `shouldBe` 8

  describe "ConfigParser" $ do
    it "doc day du cac khoa" $
      parseConfigText "c" "# note\nautosave = 3\nsave_file = my save.txt\nreport = localhost:9000\n"
        `shouldBe` Right (RuntimeConfig 3 "my save.txt" (Just ("localhost", "9000")))
    it "file rong => cau hinh mac dinh" $
      parseConfigText "c" "" `shouldBe` Right defaultConfig
    it "gia tri khong phai so => loi" $
      parseConfigText "c" "autosave = abc\n" `shouldSatisfy` isLeft'
    it "autosave = 0 => loi" $
      parseConfigText "c" "autosave = 0\n" `shouldSatisfy` isLeft'
    it "khoa la => loi" $
      parseConfigText "c" "speed = 3\n" `shouldSatisfy` isLeft'

  describe "Protocol" $ do
    it "doc lenh STATS" $
      parseCommand "STATS" `shouldBe` Right QueryStats
    it "doc lenh PING" $
      parseCommand "PING" `shouldBe` Right Ping

    it "encode lenh PING" $
      encodeCommand Ping `shouldBe` "PING"

    it "encode va parse lai lenh PING" $ do
      parseCommand (encodeCommand Ping) `shouldBe` Right Ping

    it "PING tra ve PONG va khong thay doi bang diem" $ do
      let board = mempty :: Scoreboard
      handleCommand Ping board `shouldBe` ("PONG", board)

    it "doc lenh RESULT" $
      parseCommand "RESULT 2 P1 35" `shouldBe` Right (Report (MatchResult Map2 (Winner P1) 35))
    it "doc ket qua hoa" $
      parseCommand "RESULT 1 DRAW 10" `shouldBe` Right (Report (MatchResult Map1 Draw 10))
    it "lenh rac => loi co kieu" $
      parseCommand "HELLO" `shouldSatisfy` isLeft'
    it "thieu tham so => loi" $
      parseCommand "RESULT 1 P1" `shouldSatisfy` isLeft'
    it "encode roi parse lai ra dung gia tri (QuickCheck)" $
      property $ forAll genMatchResult $ \r ->
        parseCommand (encodeCommand (Report r)) == Right (Report r)

isLeft' :: Either a b -> Bool
isLeft' (Left _) = True
isLeft' _        = False

genMatchResult :: Gen MatchResult
genMatchResult = MatchResult
  <$> elements [Map1, Map2, Map3]
  <*> elements [Winner P1, Winner P2, Draw]
  <*> choose (0, 100000)
