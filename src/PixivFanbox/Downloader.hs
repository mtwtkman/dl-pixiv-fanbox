{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

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
import PixivFanbox.Config (Config (..))
import PixivFanbox.Req (sessionIdCookieHeader)
import PixivFanbox.Util (chunkedList)
import Text.Printf (printf)
import Text.URI (mkURI)

headers :: Config -> Option 'Https
headers config =
  header "authority" "downloads.fanbox.cc"
    <> header "accept" "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.7"
    <> header "cache-control" "no-cache"
    <> sessionIdCookieHeader config

data DestInfo = DestInfo
  { destRoot :: Text,
    postTitle :: Text,
    creatorName :: Text
  }
  deriving (Show, Eq)

buildDestDirPath :: DestInfo -> Text
buildDestDirPath DestInfo {..} = destRoot <> "/" <> creatorName <> "/" <> postTitle

buildDestPath :: Text -> PostImage -> Int -> Text
buildDestPath destDirPath postImage page =
  let filename = Tx.pack $ printf "%03d" page
   in destDirPath
        <> "/"
        <> filename
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

retrieveImage :: Config -> DestInfo -> PostImage -> Int -> IO Text
retrieveImage config destInfo postImage page = do
  let destDirPath = buildDestDirPath destInfo
  let destPath = buildDestPath destDirPath postImage page
  retrieve config (toStrict $ postImageOriginalUrl postImage) destPath
  return destPath

retrieveImageChunk :: Config -> DestInfo -> [PostImage] -> IO [Text]
retrieveImageChunk config destInfo postImages = do
  let chunkSize = 3
  foldr
    ( \chunk dests -> dests <> mapConcurrently (\(page, image) -> retrieveImage config destInfo image page) chunk
    )
    (return [])
    (chunkedList chunkSize (zip [1 ..] postImages))
