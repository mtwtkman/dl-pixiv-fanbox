{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module PixivFanbox.Api where

import PixivFanbox.Req (sessionIdCookieHeader)
import Control.Monad.Catch (MonadThrow)
import qualified Data.Text.Lazy as Tx
import Network.HTTP.Req (Option, Scheme (Https), header)
import PixivFanbox.Config (Config (..))
import Text.URI (URI, mkURI)

apiUrlBase :: Tx.Text
apiUrlBase = "https://api.fanbox.cc/"

buildApiUri :: MonadThrow m => Tx.Text -> m URI
buildApiUri = mkURI . Tx.toStrict . (apiUrlBase <>)

refererHeader :: Option 'Https
refererHeader = header "referer" "https://www.fanbox.cc/"

originHeader :: Option 'Https
originHeader = header "origin" "https://www.fanbox.cc"

acceptHeader :: Option 'Https
acceptHeader = header "accept" "application/json, text/plain, */*"

authorityHeader :: Option 'Https
authorityHeader = header "authority" "api.fanbox.cc"

pragmaHeader :: Option 'Https
pragmaHeader = header "pragma" "no-cache"

basicHeaders :: Config -> Option 'Https
basicHeaders config =
  refererHeader
    <> originHeader
    <> acceptHeader
    <> authorityHeader
    <> sessionIdCookieHeader config
