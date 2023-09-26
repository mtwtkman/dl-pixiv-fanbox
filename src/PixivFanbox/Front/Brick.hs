{-# LANGUAGE TemplateHaskell #-}

module PixivFanbox.Front.Brick where

import qualified Brick.AttrMap as A
import Brick.BChan (newBChan)
import qualified Brick.Focus as F
import qualified Brick.Main as M
import qualified Brick.Types as T
import Brick.Util (bg, on, style)
import qualified Brick.Widgets.Center as C
import Brick.Widgets.Core
  ( hLimit,
    modifyDefAttr,
    str,
    vBox,
    (<+>),
  )
import qualified Brick.Widgets.Dialog as D
import qualified Brick.Widgets.Edit as E
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Graphics.Vty as V
import Lens.Micro ((^?!))
import Lens.Micro.Mtl (zoom, (%=), (.=))
import Lens.Micro.TH (makeLenses)
import PixivFanbox.Api.Entity (SupportingCreator)
import PixivFanbox.Config (Config (configSessionId), configFromString)

data SessionIdForm
  = SessionIdInputField
  | SessionIdSaveButton
  | SessionIdCancelButton
  deriving (Show, Eq, Ord)

data SupportingCreatorForm
  = SupportingCreatorListField
  | SupportingCreatorSearchField
  deriving (Show, Eq, Ord)

data PostForm
  = PostListField
  | PostSearchField
  deriving (Show, Eq, Ord)

newtype ConfiguringResource = SessionIdForm SessionIdForm
  deriving (Show, Eq, Ord)

data ExploreringResource
  = SupportingCreatorForm SupportingCreatorForm
  | PostForm PostForm
  deriving (Show, Eq, Ord)

data ResourceName
  = ConfiguringResource ConfiguringResource
  | ExploreringResource ExploreringResource
  deriving (Show, Eq, Ord)

data ConfigureEvent
  = SaveSessionId
  | CancelEditSessionId
  deriving (Show)

data ExplorerEvent
  = Download
  | GoToConfig
  deriving (Show)

data Event
  = ConfigureEvent ConfigureEvent
  | ExplorerEvent ExplorerEvent
  deriving (Show)

data ConfiguringChoice = Save | Cancel
  deriving (Show)

data State
  = Explorering
      { _config :: Config,
        _exploreringFocusRing :: F.FocusRing ResourceName,
        _selectedSupportingCreator :: Maybe SupportingCreator,
        _knownSupportingCreators :: [SupportingCreator]
      }
  | Configuring
      { _configuringFocusRing :: F.FocusRing ResourceName,
        _sessionEdit :: E.Editor String ResourceName,
        _currentConfig :: Maybe Config,
        _configureButtons :: D.Dialog ConfiguringChoice ResourceName,
        _fixSessionIdEditing :: Bool
      }

makeLenses ''State

initialState :: Maybe Config -> State
initialState Nothing = startConfiguring Nothing
initialState (Just c) =
  Explorering
    c
    ( F.focusRing
        [ ExploreringResource (SupportingCreatorForm SupportingCreatorSearchField),
          ExploreringResource (PostForm PostSearchField)
        ]
    )
    Nothing
    []

startConfiguring :: Maybe Config -> State
startConfiguring c =
  Configuring
    ( F.focusRing
        [ ConfiguringResource (SessionIdForm SessionIdInputField),
          ConfiguringResource (SessionIdForm SessionIdSaveButton),
          ConfiguringResource (SessionIdForm SessionIdCancelButton)
        ]
    )
    ( E.editor (ConfiguringResource (SessionIdForm SessionIdInputField)) (Just 1) (maybe "" (B.unpack . configSessionId) c)
    )
    c
    (D.dialog Nothing (Just (ConfiguringResource (SessionIdForm SessionIdSaveButton), choices)) 50)
    False
  where
    choices =
      [ ("Save", ConfiguringResource (SessionIdForm SessionIdSaveButton), Save),
        ("Cancel", ConfiguringResource (SessionIdForm SessionIdCancelButton), Cancel)
      ]

drawUI :: State -> [T.Widget ResourceName]
drawUI s@(Configuring {}) = [ui]
  where
    e =
      F.withFocusRing
        (s ^?! configuringFocusRing)
        (E.renderEditor (str . unlines))
        (s ^?! sessionEdit)
    ui =
      C.center $
        if s ^?! fixSessionIdEditing
          then D.renderDialog (s ^?! configureButtons) (str "Session Id")
          else str "session id: " <+> modifyDefAttr (const $ style V.underline) (hLimit 50 e)
drawUI _s@(Explorering {}) = [ui]
  where
    ui = vBox [str "logged in"]

handleConfiguring :: State -> T.BrickEvent ResourceName Event -> T.EventM ResourceName State ()
handleConfiguring (Explorering {}) _ = error "invalid handler"
handleConfiguring s@(Configuring {}) ev = do
  case (s ^?! fixSessionIdEditing, ev) of
    (True, T.VtyEvent (V.EvKey V.KEnter [])) ->
      case F.focusGetCurrent (s ^?! configuringFocusRing) of
        Just (ConfiguringResource (SessionIdForm SessionIdSaveButton)) -> do
          let config' = configFromString $ head $ E.getEditContents (s ^?! sessionEdit)
          T.put (initialState (Just config'))
        Just (ConfiguringResource (SessionIdForm SessionIdCancelButton)) -> do
          T.put (initialState Nothing)
        _ -> return ()
    (False, T.VtyEvent (V.EvKey V.KEnter [])) -> do
      fixSessionIdEditing .= True
    _ -> return ()

-- (Just (ConfiguringResource (SessionIdForm _)), T.VtyEvent e) -> zoom configureButtons $ D.handleDialogEvent e
-- _ -> zoom sessionEdit $ E.handleEditorEvent ev

handleExploering :: State -> T.BrickEvent ResourceName Event -> T.EventM ResourceName State ()
handleExploering (Configuring {}) _ = error "invalid handler"
handleExploering s@(Explorering {}) ev = do
  case ev of
    (T.VtyEvent (V.EvKey (V.KChar 'c') [V.MCtrl])) -> do
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
    [ (D.dialogAttr, V.white `on` V.blue),
      (D.buttonAttr, V.black `on` V.white),
      (D.buttonSelectedAttr, bg V.yellow)
    ]

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
  _ <- M.customMain initialVty buildVty (Just chan) app (initialState c)
  return ()
