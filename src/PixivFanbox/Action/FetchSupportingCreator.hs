module PixivFanbox.Action.FetchSupportingCreator where

import PixivFanbox.Api.Entity (SupportingCreator)
import PixivFanbox.Api.Plan.ListSupporting (Response (creators), get)
import PixivFanbox.Config (Config)
import PixivFanbox.Action (respMap)

perform :: Config -> IO [SupportingCreator]
perform config = respMap [] creators (get config)
