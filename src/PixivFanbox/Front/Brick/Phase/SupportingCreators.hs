{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module PixivFanbox.Front.Brick.Phase.SupportingCreators where

import qualified Brick.AttrMap as A
import qualified Brick.Main as M
import qualified Brick.Types as T
import Brick.Util (on)
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import Brick.Widgets.Core
  ( hLimit,
    str,
    vBox,
    vLimit,
  )
import Brick.Widgets.List as L
import Data.Text.Lazy (unpack)
import qualified Data.Vector as Vec
import qualified Graphics.Vty as V
import Lens.Micro ((^.))
import Lens.Micro.TH (makeLenses)
import PixivFanbox.Api.Entity (SupportingCreator (..), User (..))
import qualified PixivFanbox.Api.Plan.ListSupporting as ListSupportingApi
import PixivFanbox.Config (Config (..))

data Name = CreatorList deriving (Show, Eq, Ord)

data State = State
  { _items :: L.List Name SupportingCreator,
    _config :: Config
  }
  deriving (Show)

makeLenses ''State

initialState :: Int -> Config -> IO State
initialState h appConfig = do
  resp <- ListSupportingApi.get appConfig
  case resp of
    Left err -> fail $ show err
    Right body -> do
      return $ State (L.list CreatorList (Vec.fromList $ ListSupportingApi.creators body) h) appConfig

drawUI :: State -> [T.Widget Name]
drawUI state = [ui]
  where
    label = str "supporgin creators"
    box =
      B.borderWithLabel label $
        hLimit 25 $
          vLimit 15 $
            L.renderList listDrawElements True (state ^. items)

    ui =
      C.center $
        vBox
          [ C.hCenter box
          ]

listDrawElements :: Bool -> SupportingCreator -> T.Widget Name
listDrawElements selected creator =
  C.hCenter $ str (unpack . userName . supportingCreatorUser $ creator)

appEvent :: T.BrickEvent Name e -> T.EventM Name State ()
appEvent (T.VtyEvent (V.EvKey V.KEnter [])) = M.halt
appEvent _ = return ()

appMap :: A.AttrMap
appMap =
  A.attrMap
    V.defAttr
    [ (L.listAttr, V.white `on` V.blue),
      (L.listSelectedAttr, V.blue `on` V.white)
    ]

app :: M.App State e Name
app =
  M.App
    { M.appDraw = drawUI,
      M.appStartEvent = return (),
      M.appHandleEvent = appEvent,
      M.appAttrMap = const appMap,
      M.appChooseCursor = M.showFirstCursor
    }

runApp :: Config -> IO SupportingCreator
runApp appConfig = do
  st <- initialState 10 appConfig
  state <- M.defaultMain app st
  let selected = L.listSelectedElement $ state ^. items
  case selected of
    Nothing -> fail "something wrong"
    Just (_, c) -> return c
