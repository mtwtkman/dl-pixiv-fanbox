{-# LANGUAGE TemplateHaskell #-}

module PixivFanbox.Front.Brick where

import qualified Brick.AttrMap as A
import Brick.BChan (newBChan)
import qualified Brick.Focus as F
import qualified Brick.Main as M
import qualified Brick.Types as T
import Brick.Util (style)
import qualified Brick.Widgets.Center as C
import Brick.Widgets.Core (hLimit, modifyDefAttr, str, (<+>))
import qualified Brick.Widgets.Edit as E
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Graphics.Vty as V
import Lens.Micro ((^?!))
import Lens.Micro.Mtl (zoom)
import Lens.Micro.TH (makeLenses)
import PixivFanbox.Api.Entity (SupportingCreator)
import PixivFanbox.Config (Config (configSessionId))

data SettingForm
  = SessionIdInputField
  | SessionIdModificationSave
  | SessionIdModificationCancel
  deriving (Show, Eq, Ord)

data SupportingCreatorForm
  = SupportingCreatorListField
  | SupportingCreatorSearchField
  deriving (Show, Eq, Ord)

data PostForm
  = PostListField
  | PostSearchField
  deriving (Show, Eq, Ord)

data ResourceName
  = Setting SettingForm
  | SupportingCreator SupportingCreatorForm
  | Post PostForm
  deriving (Show, Eq, Ord)

data Event
  = MakeAppConfig
  | FetchSupportingCreators
  | FetchPosts
  | FilterSupportingCreators
  | FilterPosts
  | Download

data State
  = Explorering
      { _config :: Config,
        _exploreringFocusRing :: F.FocusRing ResourceName,
        _selectedSupportingCreator :: Maybe SupportingCreator,
        _knownSupportingCreators :: [SupportingCreator]
      }
  | Configuring
      { _configureringFocusRing :: F.FocusRing ResourceName,
        _sessionEdit :: E.Editor String ResourceName,
        _currentConfig :: Maybe Config
      }
  deriving (Show)

makeLenses ''State

initialState :: Maybe Config -> State
initialState Nothing = startConfiguring Nothing
initialState (Just c) =
  Explorering
    c
    ( F.focusRing
        [ SupportingCreator SupportingCreatorSearchField,
          Post PostSearchField
        ]
    )
    Nothing
    []

startConfiguring :: Maybe Config -> State
startConfiguring c =
  Configuring
    ( F.focusRing [Setting SessionIdInputField]
    )
    ( E.editor (Setting SessionIdInputField) (Just 1) (maybe "" (B.unpack . configSessionId) c)
    )
    c

drawUI :: State -> [T.Widget ResourceName]
drawUI s@(Configuring {}) = [ui]
  where
    e =
      F.withFocusRing
        (s ^?! configureringFocusRing)
        (E.renderEditor (str . unlines))
        (s ^?! sessionEdit)
    ui = C.center $ str "session id: " <+> modifyDefAttr (const $ style V.underline) (hLimit 50 e)
drawUI (Explorering {}) = [ui]
  where
    ui = str "logged in"

handleConfiguring :: State -> T.BrickEvent ResourceName Event -> T.EventM ResourceName State ()
handleConfiguring (Explorering {}) _ = error "invalid handler"
handleConfiguring _s@(Configuring {}) ev = do
  zoom sessionEdit $ E.handleEditorEvent ev

handleExploering :: State -> T.BrickEvent ResourceName Event -> T.EventM ResourceName State ()
handleExploering (Configuring {}) _ = error "invalid handler"
handleExploering s@(Explorering {}) ev = do
  case ev of
    (T.VtyEvent (V.EvKey (V.KChar 's') [])) -> do
      T.put (startConfiguring (Just $ s ^?! config))
      return ()
    _ -> return ()

appEvent :: T.BrickEvent ResourceName Event -> T.EventM ResourceName State ()
appEvent (T.VtyEvent (V.EvKey V.KEsc [])) = M.halt
appEvent ev = do
  s <- T.get
  case s of
    Configuring {} -> handleConfiguring s ev
    Explorering {} -> handleExploering s ev

appMap :: A.AttrMap
appMap =
  A.attrMap
    V.defAttr
    []

appCursor :: State -> [T.CursorLocation ResourceName] -> Maybe (T.CursorLocation ResourceName)
appCursor = M.showFirstCursor

app :: M.App State Event ResourceName
app =
  M.App
    { M.appDraw = drawUI,
      M.appStartEvent = return (),
      M.appHandleEvent = appEvent,
      M.appAttrMap = const appMap,
      M.appChooseCursor = appCursor
    }

runApp :: Maybe Config -> IO ()
runApp c = do
  chan <- newBChan 10
  let buildVty = V.mkVty V.defaultConfig
  initialVty <- buildVty
  finalState <- M.customMain initialVty buildVty (Just chan) app (initialState c)
  print finalState
  return ()
