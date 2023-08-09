{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module PixivFanbox.Api.PostListCreator where

import Data.Aeson (FromJSON (..), withObject, (.:))
import Data.Maybe (fromJust)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as Tx
import GHC.Generics (Generic)
import Network.HTTP.Req
  ( GET (GET),
    NoReqBody (NoReqBody),
    Req,
    defaultHttpConfig,
    jsonResponse,
    req,
    responseBody,
    runReq,
    useHttpsURI,
  )
import PixivFanbox.Api (basicHeaders, buildApiUri)
import PixivFanbox.Api.Entity (PostItem)
import PixivFanbox.Config (Config)
import Text.URI (URI)

apiUrl :: Int -> Text -> Req URI
apiUrl limit creatorId =
  buildApiUri $
    "post.listCreator?creatorId="
      <> creatorId
      <> "&maxPublishedDatetime=9999-12-31+00:00:00&maxId=999999999999&limit="
      <> Tx.pack (show limit)

newtype Response = Response
  { items :: [PostItem]
  }
  deriving (Show, Eq, Generic)

instance FromJSON Response where
  parseJSON = withObject "Response" $ \outer -> do
    inner <- outer .: "body"
    Response <$> inner .: "items"

get :: Int -> Text -> Config -> IO Response
get limit creatorId config = runReq defaultHttpConfig $ do
  uri <- apiUrl limit creatorId
  let (url, uriOptions) = fromJust (useHttpsURI uri)
  let options = basicHeaders config <> uriOptions
  resp <- req GET url NoReqBody jsonResponse options
  return $ responseBody resp
