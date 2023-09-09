module PixivFanbox.Action.SearchCreator where

import Data.Text.Lazy (Text)
import PixivFanbox.Action (respMap)
import PixivFanbox.Api.Creator.Search (Response (creators), get)
import PixivFanbox.Api.Entity (FoundCreator)
import PixivFanbox.Config (Config)

perform :: Text -> Int -> Config -> IO [FoundCreator]
perform query page config = respMap [] creators (get query page config)
