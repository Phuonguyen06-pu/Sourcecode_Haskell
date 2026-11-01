-- | Kiểm thử luồng màn hình, nút bấm (UI.Layout), phím điều khiển và tạm dừng.
module InputSpec (spec) where

import Graphics.Gloss.Interface.IO.Game
import Test.Hspec

import Engine.Input (handleEvent)
import UI.Events (fromGlossEvent)
import Levels.Maps  (mapLayout)
import Model.Types
import Model.Hp (hpValue)
import Model.World  (initGameState, spawnP1, spawnP2)
import UI.Layout

-- | Tâm của một nút trên một màn hình (lấy từ UI.Layout, cùng nguồn với phần vẽ).
centerOf :: ScreenState -> ButtonId -> (Float, Float)
centerOf s i = case [btnCenter b | b <- buttonsFor s, btnId b == i] of
  (c : _) -> c
  []      -> (9999, 9999)

mods :: Modifiers
mods = Modifiers Up Up Up

click :: (Float, Float) -> GameState -> GameState
click pos =
  handleEvent mapLayout
    (fromGlossEvent (EventKey (MouseButton LeftButton) Down mods pos))

clickBtn :: ButtonId -> GameState -> GameState
clickBtn i gs = click (centerOf (screen gs) i) gs

key :: Key -> KeyState -> GameState -> GameState
key k ks =
  handleEvent mapLayout
    (fromGlossEvent (EventKey k ks mods (0, 0)))

menu :: GameState
menu = initGameState []

-- | Đang chơi map 1 (sân ngẫu nhiên).
inMatch :: GameState
inMatch = clickBtn BtnStart (clickBtn BtnPlay menu)

spec :: Spec
spec = describe "Input & man hinh" $ do

  describe "dieu huong menu" $ do
    it "bam 2 PLAYERS -> man chon map" $
      screen (clickBtn BtnPlay menu) `shouldBe` MapSelectScreen
    it "bam HELP -> man huong dan, BACK -> ve menu" $ do
      let h = clickBtn BtnHelp menu
      screen h `shouldBe` HelpScreen
      screen (clickBtn BtnBack h) `shouldBe` MenuScreen
    it "bam ngoai moi nut thi khong doi gi" $
      click (400, 250) menu `shouldBe` menu
    it "chon map roi quay lai menu van giu map da chon" $ do
      let s = clickBtn (BtnMap Map3) (clickBtn BtnPlay menu)
      mapId s `shouldBe` Map3
      mapId (clickBtn BtnBack s) `shouldBe` Map3

  describe "bat dau tran" $ do
    it "chon map 2 roi PLAY -> PlayScreen, du vat can, san dac biet va hai xe o diem xuat phat" $ do
      let g = clickBtn BtnStart (clickBtn (BtnMap Map2) (clickBtn BtnPlay menu))
      screen g `shouldBe` PlayScreen
      mapId g `shouldBe` Map2
      walls g `shouldSatisfy` (not . null)
      length [() | Floor _ (Teleporter _) <- floors g] `shouldBe` 2
      tankPos (tank1 g) `shouldBe` spawnP1
      tankPos (tank2 g) `shouldBe` spawnP2
    it "moi tran moi la mot san khac (ngau nhien)" $ do
      let g1 = inMatch
          g2 = clickBtn BtnNewMap (key (SpecialKey KeyEsc) Down g1)
      screen g2 `shouldBe` PlayScreen
      walls g1 `shouldSatisfy` (/= walls g2)
    it "REMATCH tu man ket thuc: dat lai mau va giu nguyen map" $ do
      let over = (clickBtn (BtnMap Map3) (clickBtn BtnPlay menu))
                   { screen = GameOverScreen (Winner P1)
                   , tank1  = mkTank spawnP1 0 0 }
          g = clickBtn BtnRematch over
      screen g `shouldBe` PlayScreen
      mapId g `shouldBe` Map3
      hpValue (tankHp (tank1 g)) `shouldBe` 100
    it "CHANGE MAP va MAIN MENU tu man ket thuc" $ do
      let over = inMatch { screen = GameOverScreen Draw }
      screen (clickBtn BtnChangeMap over) `shouldBe` MapSelectScreen
      screen (clickBtn BtnMainMenu over) `shouldBe` MenuScreen
    it "phim M ve menu tu man dang choi" $
      screen (key (Char 'm') Down inMatch) `shouldBe` MenuScreen

  describe "tam dung (ESC)" $ do
    it "ESC khi dang choi -> PauseScreen, ESC lan nua -> choi tiep" $ do
      let p = key (SpecialKey KeyEsc) Down inMatch
      screen p `shouldBe` PauseScreen
      screen (key (SpecialKey KeyEsc) Down p) `shouldBe` PlayScreen
    it "ESC o menu khong lam gi" $
      key (SpecialKey KeyEsc) Down menu `shouldBe` menu
    it "nut PAUSE tren man choi va nut RESUME" $ do
      let p = clickBtn BtnPause inMatch
      screen p `shouldBe` PauseScreen
      screen (clickBtn BtnResume p) `shouldBe` PlayScreen
    it "QUIT TO MENU khi dang tam dung" $
      screen (clickBtn BtnQuit (clickBtn BtnPause inMatch)) `shouldBe` MenuScreen
    it "khi tam dung thi phim ban khong tao dan" $ do
      let p = key (SpecialKey KeyEsc) Down inMatch
      bullets (key (SpecialKey KeySpace) Down p) `shouldBe` []

  describe "phim dieu khien" $ do
    it "nhan phim P1 bat dau di, tha phim thi bo" $ do
      let down = key (Char 'w') Down inMatch
          up   = key (Char 'w') Up down
      activeKeys down `shouldBe` [P1Forward]
      activeKeys up `shouldBe` []
    it "hai nguoi giu phim cung luc" $ do
      let g = key (SpecialKey KeyUp) Down (key (Char 'w') Down inMatch)
      activeKeys g `shouldSatisfy` (\ks -> P1Forward `elem` ks && P2Forward `elem` ks)
    it "SPACE ban 2 vien dan, ENTER ban 2 vien dan cua P2" $ do
      length (bullets (key (SpecialKey KeySpace) Down inMatch)) `shouldBe` 2
      let g = key (SpecialKey KeyEnter) Down inMatch
      length (bullets g) `shouldBe` 2
      all ((== P2) . bulletOwner) (bullets g) `shouldBe` True
  describe "thong bao loi (#17)" $ do

    it "luu duoc thong bao loi trong GameState" $ do
      let g = menu { notice = Just "Map load failed" }
      notice g `shouldBe` Just "Map load failed"

    it "nhan phim thi xoa thong bao" $ do
      let g = menu { notice = Just "Map load failed" }
          result = key (Char 'w') Down g

      notice result `shouldBe` Nothing

    it "click chuot thi xoa thong bao" $ do
      let g = menu { notice = Just "Map load failed" }
          result = click (400, 250) g

      notice result `shouldBe` Nothing

    it "tha phim khong xoa thong bao" $ do
      let g = menu { notice = Just "Map load failed" }
          result = key (Char 'w') Up g

      notice result `shouldBe` Just "Map load failed"

    it "xoa thong bao khong anh huong dieu khien xe" $ do
      let g = inMatch { notice = Just "Map load failed" }
          result = key (Char 'w') Down g

      notice result `shouldBe` Nothing
      activeKeys result `shouldBe` [P1Forward]
