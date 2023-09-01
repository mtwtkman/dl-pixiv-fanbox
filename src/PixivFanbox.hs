module PixivFanbox
  ( runBrickApp,
    fromConfigFile,
  )
where

import PixivFanbox.Config (Config, fromConfigFile)
import qualified PixivFanbox.Front.Brick as BrickUI

runBrickApp :: Maybe Config -> IO ()
runBrickApp = BrickUI.runApp
