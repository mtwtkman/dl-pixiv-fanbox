{-# LANGUAGE TemplateHaskell #-}

module PixivFanbox.Front.Brick where

import qualified Brick.AttrMap as A
import Brick.BChan (newBChan)
import qualified Brick.Focus as F
import qualified Brick.Main as M
import qualified Brick.Types as T
import qualified Brick.Widgets.Center as C
import Brick.Widgets.Core (str)
import qualified Brick.Widgets.Edit as E
import qualified Graphics.Vty as V
import Lens.Micro ((^?!))
import Lens.Micro.TH (makeLenses)
import PixivFanbox.Api.Entity (SupportingCreator)
import PixivFanbox.Config (Config)

data SettingForm = SessionIdInputField deriving (Show, Eq, Ord)

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
        _session :: E.Editor String ResourceName,
        _currentConfig :: Maybe Config
      }
  deriving (Show)

makeLenses ''State

initialState :: Maybe Config -> State
initialState Nothing =
  Configuring
    ( F.focusRing [Setting SessionIdInputField]
    )
    ( E.editor (Setting SessionIdInputField) (Just 1) ""
    )
    Nothing
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

drawUI :: State -> [T.Widget ResourceName]
drawUI s@(Configuring {}) = [ui]
  where
    c = F.withFocusRing (s ^?! configureringFocusRing) (E.renderEditor (str . unlines)) (s ^?! session)
    ui = C.center $ str "session id: "
drawUI (Explorering {}) = [ui]
  where
    ui = str "logged in"

appEvent :: T.BrickEvent ResourceName Event -> T.EventM ResourceName State ()
appEvent (T.VtyEvent (V.EvKey V.KEsc [])) = M.halt
appEvent (T.VtyEvent (V.EvKey (V.KChar 'q') [])) = M.halt
appEvent (T.VtyEvent (V.EvKey (V.KChar 's') [])) = do
  currentState <- T.get
  case currentState of
    Configuring {} -> return ()
    Explorering {} -> do
      T.put (initialState Nothing)
      return ()
appEvent _ = return ()

appMap :: A.AttrMap
appMap = A.attrMap V.defAttr []

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
