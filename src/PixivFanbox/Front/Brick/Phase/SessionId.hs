{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module PixivFanbox.Front.Brick.Phase.SessionId where

import qualified Brick.AttrMap as A
import Brick.BChan (BChan, newBChan, writeBChan)
import qualified Brick.Focus as F
import qualified Brick.Main as M
import qualified Brick.Types as T
import qualified Brick.Widgets.Center as C
import Brick.Widgets.Core
  ( hLimit,
    str,
    vBox,
    (<+>),
  )
import qualified Brick.Widgets.Edit as E
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Graphics.Vty as V
import Lens.Micro ((^.))
import Lens.Micro.Mtl (use, zoom, (.=))
import Lens.Micro.TH (makeLenses)
import qualified PixivFanbox.Action.ValidateSessionId as SessionIdValidator

data Event = RequestSessionIdValidation
  deriving (Show, Eq)

data Name = Edit deriving (Show, Eq, Ord)

data State = State
  { _focusRing :: F.FocusRing Name,
    _edit :: E.Editor String Name,
    _channel :: BChan Event
  }

makeLenses ''State

drawUI :: State -> [T.Widget Name]
drawUI st = [ui]
  where
    editor = F.withFocusRing (st ^. focusRing) (E.renderEditor (str . unlines)) (st ^. edit)
    ui =
      C.center $
        vBox
          [ str "Session Id: " <+> hLimit 40 editor
          ]

newEditor :: E.Editor String Name
newEditor = E.editor Edit (Just 1) ""

initialState :: BChan Event -> State
initialState = State (F.focusRing [Edit]) newEditor

validateSessionId :: BChan Event -> IO ()
validateSessionId = flip writeBChan RequestSessionIdValidation

appEvent :: T.BrickEvent Name Event -> T.EventM Name State ()
appEvent (T.AppEvent RequestSessionIdValidation) = do
  sesid <- use edit
  case head $ E.getEditContents sesid of
    [] -> do
      return ()
    v -> do
      isValid <- liftIO $ SessionIdValidator.perform (BS.pack v)
      if isValid
        then do
          M.halt
        else do
          edit .= newEditor
          return ()
appEvent (T.VtyEvent (V.EvKey V.KEnter [])) = do
  chan <- use channel
  liftIO $ validateSessionId chan
appEvent ev = do
  r <- use focusRing
  case F.focusGetCurrent r of
    Just Edit -> zoom edit $ E.handleEditorEvent ev
    _ -> return ()

appCursor :: State -> [T.CursorLocation Name] -> Maybe (T.CursorLocation Name)
appCursor = F.focusRingCursor (^. focusRing)

appMap :: A.AttrMap
appMap = A.attrMap V.defAttr []

app :: M.App State Event Name
app =
  M.App
    { M.appDraw = drawUI,
      M.appStartEvent = return (),
      M.appHandleEvent = appEvent,
      M.appAttrMap = const appMap,
      M.appChooseCursor = appCursor
    }

runApp :: IO BS.ByteString
runApp = do
  chan <- newBChan 1
  let buildVty = V.mkVty V.defaultConfig
  initialVty <- buildVty
  finalState <- M.customMain initialVty buildVty (Just chan) app (initialState chan)
  let contents = E.getEditContents $ finalState ^. edit
  case contents of
    x : _ -> return $ BS.pack x
    [] -> fail "something wrong"
