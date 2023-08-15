{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module PixivFanbox.Api where

import Control.Exception (try)
import Control.Monad.Catch (MonadThrow)
import Data.Aeson (FromJSON)
import Data.Maybe (fromJust)
import qualified Data.Text.Lazy as Tx
import Network.HTTP.Req (HttpException, Option, Scheme (Https), header, useHttpsURI)
import PixivFanbox.Config (Config (..))
import PixivFanbox.Req (jsonRequest, sessionIdCookieHeader)
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

type ApiResponse a = Either HttpException a

performGet :: FromJSON m => Config -> URI -> IO (ApiResponse m)
performGet config uri = do
  let (url, uriOptions) = fromJust (useHttpsURI uri)
  let options = basicHeaders config <> uriOptions
  try $ jsonRequest url options
