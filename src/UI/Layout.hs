-- quản lý danh sách nút bấm của từng màn hình, xác định nút bấm dựa trên tọa độ bấm

module UI.Layout
  ( ButtonId (..)
  , Button (..)
  , buttonsFor
  , buttonAt
  ) where

import Data.Maybe (listToMaybe)

import Model.Types (ScreenState (..), MapId (..))

data ButtonId
  = BtnPlay | BtnHelp | BtnBack
  | BtnMap MapId | BtnStart
  | BtnPause | BtnResume | BtnNewMap | BtnQuit
  | BtnRematch | BtnChangeMap | BtnMainMenu
  deriving (Show, Eq)

data Button = Button
  { btnId     :: ButtonId
  , btnCenter :: (Float, Float)
  , btnSize   :: (Float, Float)
  , btnLabel  :: String
  } deriving (Show, Eq)


buttonsFor :: ScreenState -> [Button]
buttonsFor MenuScreen =
  [ Button BtnPlay (0, 20)    (200, 60) "2 PLAYERS"
  , Button BtnHelp (0, -70)   (200, 50) "HELP"
  ]
buttonsFor HelpScreen =
  [ Button BtnBack (0, -200)  (200, 50) "BACK" ]
buttonsFor MapSelectScreen =
  [ Button (BtnMap Map1) (-250, 75) (200, 50) "MAP 1"
  , Button (BtnMap Map2) (0, 75)    (200, 50) "MAP 2"
  , Button (BtnMap Map3) (250, 75)  (200, 50) "MAP 3"
  , Button BtnStart   (0, -240)  (240, 50) "PLAY"
  , Button BtnBack    (-380, -240) (160, 50) "BACK"
  ]
buttonsFor PlayScreen =
  [ Button BtnPause (0, 292) (130, 24) "PAUSE (ESC)" ]
buttonsFor PauseScreen =
  [ Button BtnResume (0, 40)   (260, 50) "RESUME"
  , Button BtnNewMap (0, -25)  (260, 50) "NEW RANDOM MAP"
  , Button BtnQuit   (0, -90)  (260, 50) "QUIT TO MENU"
  ]
buttonsFor (GameOverScreen _) =
  [ Button BtnRematch   (0, -130) (240, 44) "REMATCH"
  , Button BtnChangeMap (0, -185) (240, 44) "CHANGE MAP"
  , Button BtnMainMenu  (0, -240) (240, 44) "MAIN MENU"
  ]

buttonAt :: ScreenState -> (Float, Float) -> Maybe ButtonId
buttonAt s (x, y) = listToMaybe [btnId b | b <- buttonsFor s, inside b]
  where
    inside b = let (cx, cy) = btnCenter b
                   (w, h)   = btnSize b
               in abs (x - cx) <= w / 2 && abs (y - cy) <= h / 2
