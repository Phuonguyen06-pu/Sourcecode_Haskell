module EventsSpec (spec) where

import Graphics.Gloss.Interface.IO.Game
import Test.Hspec

import Model.Types (Control(..))
import UI.Events (UiEvent(..), fromGlossEvent)

mods :: Modifiers
mods = Modifiers Up Up Up

keyEvent :: Key -> KeyState -> UiEvent
keyEvent k ks =
  fromGlossEvent (EventKey k ks mods (0, 0))

spec :: Spec
spec = describe "UI.Events" $ do

  it "click chuot trai -> UiClick" $
    fromGlossEvent
      (EventKey (MouseButton LeftButton) Down mods (100, 50))
      `shouldBe` UiClick (100, 50)

  it "ESC -> UiPause" $
    keyEvent (SpecialKey KeyEsc) Down `shouldBe` UiPause

  it "M -> UiMenu" $
    keyEvent (Char 'm') Down `shouldBe` UiMenu

  it "W -> nhan va tha P1Forward" $ do
    keyEvent (Char 'w') Down `shouldBe` UiPress P1Forward
    keyEvent (Char 'w') Up `shouldBe` UiRelease P1Forward

  it "SPACE -> P1Fire" $
    keyEvent (SpecialKey KeySpace) Down `shouldBe` UiPress P1Fire

  it "phim mui ten len -> P2Forward" $
    keyEvent (SpecialKey KeyUp) Down `shouldBe` UiPress P2Forward

  it "ENTER -> P2Fire" $
    keyEvent (SpecialKey KeyEnter) Down `shouldBe` UiPress P2Fire

  it "phim khong duoc gan -> UiIgnore" $
    keyEvent (Char 'x') Down `shouldBe` UiIgnore

  it "tha ESC khong kich hoat tam dung" $
    keyEvent (SpecialKey KeyEsc) Up `shouldBe` UiIgnore

  it "click chuot phai -> UiIgnore" $
    fromGlossEvent
      (EventKey (MouseButton RightButton) Down mods (0, 0))
      `shouldBe` UiIgnore