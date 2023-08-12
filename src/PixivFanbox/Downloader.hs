{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module PixivFanbox.Downloader where

import Control.Concurrent.Async (mapConcurrently)
import qualified Data.ByteString.Char8 as B
import Data.Maybe (fromJust)
import Data.Text (Text)
import qualified Data.Text as Tx
import Data.Text.Lazy (toStrict)
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
import PixivFanbox.Api.Entity (PostImage (..))
import PixivFanbox.Config (Config)
import PixivFanbox.Req (sessionIdCookieHeader)
import PixivFanbox.Util (chunkedList)
import Text.URI (mkURI)

headers :: Config -> Option 'Https
headers config =
  header "authority" "downloads.fanbox.cc"
    <> header "accept" "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.7"
    <> header "cache-control" "no-cache"
    <> sessionIdCookieHeader config

buildDestDirPath :: Text -> Text -> Text -> Text
buildDestDirPath root userName title = root <> "/" <> userName <> "/" <> title

buildDestPath :: Text -> PostImage -> Text
buildDestPath destDirPath postImage =
  destDirPath
    <> "/"
    <> toStrict (postImageId postImage)
    <> "."
    <> toStrict (postImageExtension postImage)

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

retrieveImage :: Config -> Text -> Text -> Text -> PostImage -> IO Text
retrieveImage config destRoot title creatorName postImage = do
  let destDirPath = buildDestDirPath destRoot creatorName title
  let destPath = buildDestPath destDirPath postImage
  retrieve config (toStrict $ postImageOriginalUrl postImage) destPath
  return destPath

retrieveImageChunk :: Int -> Config -> Text -> Text -> Text -> [PostImage] -> IO [Text]
retrieveImageChunk size config destRoot title creatorName postImages =
  foldr
    ( \chunk dests -> dests <> mapConcurrently (retrieveImage config destRoot title creatorName) chunk
    )
    (return [])
    (chunkedList size postImages)
