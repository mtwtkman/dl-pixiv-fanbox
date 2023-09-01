module PixivFanbox.Action.ValidateSessionId where

import Data.ByteString.Lazy.Char8 (ByteString)
import PixivFanbox.Action (respMap)
import PixivFanbox.Api.Bell.CountUnread (get)
import PixivFanbox.Config (Config (Config))

perform :: ByteString -> IO Bool
perform sessionId = respMap False (const True) (get $ Config sessionId)
