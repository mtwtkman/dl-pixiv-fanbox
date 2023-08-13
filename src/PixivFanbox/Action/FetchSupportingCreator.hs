module PixivFanbox.Action.FetchSupportingCreator where

import Data.Functor ((<&>))
import PixivFanbox.Api.Entity (SupportingCreator)
import PixivFanbox.Api.Plan.ListSupporting (Response (creators), get)
import PixivFanbox.Config (Config)

perform :: Config -> IO [SupportingCreator]
perform config = get config <&> creators
