{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module PixivFanbox.Front.Brick.Phase.Menu where

import qualified Brick.AttrMap as A
import qualified Brick.Main as M
import qualified Brick.Types as T
import Brick.Widgets.Core (str, vBox)
import qualified Graphics.Vty as V
import Lens.Micro ((^.))
import Lens.Micro.Mtl ((.=))
import Lens.Micro.TH (makeLenses)

data Name = SelectMenuItem deriving (Show, Eq, Ord)

data Choice
  = Setting
  | SupportingCreatorList
  | Quit
  deriving (Show)

newtype State = State
  { _selected :: Choice
  }
  deriving (Show)

makeLenses ''State

drawUI :: State -> [T.Widget Name]
drawUI _ = [ui]
  where
    ui =
      vBox
        [ makeRow Quit,
          makeRow Setting,
          makeRow SupportingCreatorList
        ]

keyMap :: Choice -> Char
keyMap Quit = 'q'
keyMap Setting = 's'
keyMap SupportingCreatorList = 'c'

makeRow :: Choice -> T.Widget Name
makeRow c =
  let label = "(" <> [keyMap c] <> ")"
   in str $ displayChoice c <> label
  where
    displayChoice :: Choice -> String
    displayChoice Quit = "quit"
    displayChoice Setting = "modify setting"
    displayChoice SupportingCreatorList = "select supporting creator"

select :: Choice -> T.EventM Name State ()
select = (>> M.halt) . (selected .=)

appEvent :: T.BrickEvent Name e -> T.EventM Name State ()
appEvent (T.VtyEvent (V.EvKey (V.KChar 'q') [])) = select Quit
appEvent (T.VtyEvent (V.EvKey (V.KChar 's') [])) = select Setting
appEvent (T.VtyEvent (V.EvKey (V.KChar 'c') [])) = select SupportingCreatorList
appEvent _ = return ()

appMap :: A.AttrMap
appMap = A.attrMap V.defAttr []

app :: M.App State e Name
app =
  M.App
    { M.appDraw = drawUI,
      M.appChooseCursor = M.showFirstCursor,
      M.appHandleEvent = appEvent,
      M.appAttrMap = const appMap,
      M.appStartEvent = return ()
    }

initialState :: State
initialState = State Quit

runApp :: IO Choice
runApp = do
  state <- M.defaultMain app initialState
  return $ state ^. selected
