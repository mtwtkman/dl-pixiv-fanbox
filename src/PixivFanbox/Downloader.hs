{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module PixivFanbox.Downloader where

import Data.ByteString.Char8 (ByteString)
import Data.Text (Text)
import Network.HTTP.Req
  ( GET (GET),
    NoReqBody (NoReqBody),
    Option,
    Scheme (Https),
    bsResponse,
    defaultHttpConfig,
    header,
    https,
    req,
    responseBody,
    runReq,
  )
import PixivFanbox.Config (Config)
import PixivFanbox.Req (sessionIdCookieHeader)

headers :: Config -> Option 'Https
headers config =
  header "authority" "downloads.fanbox.cc"
    <> header "accept" "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.7"
    <> header "cache-control" "no-cache"
    <> sessionIdCookieHeader config

download :: Config -> Text -> IO ByteString
download config url = runReq defaultHttpConfig $ do
  resp <- req GET (https url) NoReqBody bsResponse (headers config)
  return $ responseBody resp

retrieve :: Config -> Text -> FilePath -> IO ()
retrieve config url dest = undefined
