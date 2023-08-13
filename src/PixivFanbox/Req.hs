{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module PixivFanbox.Req where

import Data.ByteString.Lazy.Char8 (toStrict)
import Network.HTTP.Req (Option, Scheme (Https), header)
import PixivFanbox.Config (Config (..))

sessionIdCookieHeader :: Config -> Option 'Https
sessionIdCookieHeader config = header "cookie" (toStrict ("FANBOXSESSID=" <> configSessionId config))
