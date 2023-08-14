{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module PixivFanbox.Req where

import Data.Aeson (FromJSON)
import Data.ByteString.Lazy.Char8 (toStrict)
import Network.HTTP.Req
  ( GET (GET),
    NoReqBody (NoReqBody),
    Option,
    Scheme (Https),
    Url,
    defaultHttpConfig,
    header,
    jsonResponse,
    req,
    responseBody,
    runReq,
  )
import PixivFanbox.Config (Config (..))

sessionIdCookieHeader :: Config -> Option 'Https
sessionIdCookieHeader config = header "cookie" (toStrict ("FANBOXSESSID=" <> configSessionId config))

jsonRequest :: FromJSON m => Url 'Https -> Option 'Https -> IO m
jsonRequest url options = runReq defaultHttpConfig $ do
  resp <- req GET url NoReqBody jsonResponse options
  return $ responseBody resp
