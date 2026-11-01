-- chuyển sự kiện Gloss sang sự kiện UI nội bộ, tách riêng để kiểm thử
module UI.Events
  ( UiEvent(..)
  , fromGlossEvent
  ) where

import Graphics.Gloss.Interface.IO.Game
  ( Event(..), Key(..), KeyState(..)
  , MouseButton(..), SpecialKey(..)
  )

import Model.Types (Control(..))

data UiEvent
  = UiClick (Float, Float)
  | UiPause
  | UiMenu
  | UiPress Control
  | UiRelease Control
  | UiIgnore
  deriving (Show, Eq)

fromGlossEvent :: Event -> UiEvent
fromGlossEvent (EventKey (MouseButton LeftButton) Down _ pos) =
  UiClick pos

fromGlossEvent (EventKey (SpecialKey KeyEsc) Down _ _) =
  UiPause

fromGlossEvent (EventKey (Char c) Down _ _)
  | c `elem` "mM" = UiMenu

fromGlossEvent (EventKey k Down _ _) =
  maybe UiIgnore UiPress (keyToControl k)

fromGlossEvent (EventKey k Up _ _) =
  maybe UiIgnore UiRelease (keyToControl k)

fromGlossEvent _ = UiIgnore

-- Chuyển phím bấm Gloss sang Control nội bộ
keyToControl :: Key -> Maybe Control
keyToControl (Char c)
  | c `elem` "wW" = Just P1Forward
  | c `elem` "sS" = Just P1Back
  | c `elem` "aA" = Just P1Left
  | c `elem` "dD" = Just P1Right

keyToControl (SpecialKey KeySpace) = Just P1Fire
keyToControl (SpecialKey KeyUp)    = Just P2Forward
keyToControl (SpecialKey KeyDown)  = Just P2Back
keyToControl (SpecialKey KeyLeft)  = Just P2Left
keyToControl (SpecialKey KeyRight) = Just P2Right
keyToControl (SpecialKey KeyEnter) = Just P2Fire

keyToControl _ = Nothing