{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module PixivFanbox.Downloader where

import qualified Data.ByteString.Char8 as B
import Data.Maybe (fromJust)
import Data.Text (Text)
import qualified Data.Text as Tx
import Network.HTTP.Req
  ( GET (GET),
    NoReqBody (NoReqBody),
    Option,
    Scheme (Https),
    bsResponse,
    defaultHttpConfig,
    header,
    req,
    responseBody,
    runReq,
    useHttpsURI,
  )
import PixivFanbox.Config (Config)
import PixivFanbox.Req (sessionIdCookieHeader)
import Text.URI (mkURI)

headers :: Config -> Option 'Https
headers config =
  header "authority" "downloads.fanbox.cc"
    <> header "accept" "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.7"
    <> header "cache-control" "no-cache"
    <> sessionIdCookieHeader config

buildDestDirPath :: Text -> Text -> Text -> Text
buildDestDirPath root userName title = root <> "/" <> userName <> "/" <> title

download :: Config -> Text -> IO B.ByteString
download config escapedUrlString = runReq defaultHttpConfig $ do
  uri <- mkURI escapedUrlString
  let url = fst $ fromJust (useHttpsURI uri)
  resp <- req GET url NoReqBody bsResponse (headers config)
  return $ responseBody resp

retrieve :: Config -> Text -> Text -> IO ()
retrieve config url dest = do
  blob <- download config url
  B.writeFile (Tx.unpack dest) blob
