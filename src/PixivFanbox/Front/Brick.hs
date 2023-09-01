{-# LANGUAGE TemplateHaskell #-}

module PixivFanbox.Front.Brick where

import qualified Brick.AttrMap as A
import Brick.BChan (newBChan)
import qualified Brick.Focus as F
import qualified Brick.Main as M
import qualified Brick.Types as T
import Brick.Widgets.Core (str)
import qualified Graphics.Vty as V
import Lens.Micro.TH (makeLenses)
import PixivFanbox.Api.Entity (SupportingCreator)
import PixivFanbox.Config (Config)

data ResourceName
  = Setting
  | SupportingCreatorList
  | SupportingCreatorFilter
  | PostList
  | PostFilter
  deriving (Show, Eq, Ord)

data Event
  = MakeAppConfig
  | FetchSupportingCreators
  | FetchPosts
  | FilterSupportingCreators
  | FilterPosts
  | Download

data State
  = LoggedIn
      { _config :: Config,
        _focusRing :: F.FocusRing ResourceName,
        _selectedSupportingCreator :: Maybe SupportingCreator,
        _knownSupportingCreators :: [SupportingCreator]
      }
  | NotLoggedIn
  deriving (Show)

makeLenses ''State

initialState :: Maybe Config -> State
initialState Nothing = NotLoggedIn
initialState (Just c) =
  LoggedIn
    c
    ( F.focusRing
        [ Setting,
          SupportingCreatorFilter,
          PostFilter
        ]
    )
    Nothing
    []

drawUI :: State -> [T.Widget ResourceName]
drawUI state = [ui]
  where
    ui = str "xx"

appEvent :: T.BrickEvent ResourceName Event -> T.EventM ResourceName State ()
appEvent ev = undefined

appMap :: A.AttrMap
appMap = A.attrMap V.defAttr []

appCursor :: State -> [T.CursorLocation ResourceName] -> Maybe (T.CursorLocation ResourceName)
appCursor state = undefined

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
  putStrLn $ show finalState
  return ()
