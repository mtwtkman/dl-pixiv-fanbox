{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module PixivFanbox.Front.Brick.Phase.SessionId where

import qualified Brick.AttrMap as A
import qualified Brick.Focus as F
import qualified Brick.Main as M
import qualified Brick.Types as T
import qualified Brick.Widgets.Center as C
import Brick.Widgets.Core
  ( hLimit,
    str,
    (<+>),
  )
import qualified Brick.Widgets.Edit as E
import Data.ByteString.Lazy.Char8 (ByteString, pack)
import qualified Graphics.Vty as V
import Lens.Micro ((^.))
import Lens.Micro.Mtl (use, zoom)
import Lens.Micro.TH (makeLenses)

data Name = Edit deriving (Show, Eq, Ord)

configName :: ByteString
configName = "dl-pixiv-fanbox.ini"

data State = State
  { _focusRing :: F.FocusRing Name,
    _edit :: E.Editor String Name,
    _sessionId :: Maybe ByteString
  }
  deriving (Show)

makeLenses ''State

newtype Event = UpdateSessionId (Maybe ByteString) deriving (Show)

drawUI :: State -> [T.Widget Name]
drawUI st = [ui]
  where
    editor = F.withFocusRing (st ^. focusRing) (E.renderEditor (str . unlines)) (st ^. edit)
    ui = C.center (str "Session Id: " <+> hLimit 40 editor)

initialState :: State
initialState =
  State
    (F.focusRing [Edit])
    (E.editor Edit (Just 1) "")
    Nothing

appEvent :: T.BrickEvent Name e -> T.EventM Name State ()
appEvent (T.VtyEvent (V.EvKey V.KEnter [])) = M.halt
appEvent ev = do
  r <- use focusRing
  case F.focusGetCurrent r of
    Just Edit -> do
      zoom edit $ E.handleEditorEvent ev
    _ -> return ()

appCursor :: State -> [T.CursorLocation Name] -> Maybe (T.CursorLocation Name)
appCursor = F.focusRingCursor (^. focusRing)

appMap :: A.AttrMap
appMap = A.attrMap V.defAttr []

app :: M.App State e Name
app =
  M.App
    { M.appDraw = drawUI,
      M.appStartEvent = return (),
      M.appHandleEvent = appEvent,
      M.appAttrMap = const appMap,
      M.appChooseCursor = appCursor
    }

runApp :: IO ByteString
runApp = do
  state <- M.defaultMain app initialState
  let contents = E.getEditContents $ state ^. edit
  case contents of
    x : _ -> return $ pack x
    [] -> fail "something wrong"
