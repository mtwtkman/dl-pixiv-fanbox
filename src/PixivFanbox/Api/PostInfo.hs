{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module PixivFanbox.Api.PostInfo where

import Data.Aeson (FromJSON (..), withObject, (.:))
import Data.Maybe (fromJust)
import Data.Text.Lazy (Text)
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
import PixivFanbox.Api.Entity (PostImage, PostItem, User)
import PixivFanbox.Config (Config)
import Text.URI (URI)

apiUrl :: Text -> Req URI
apiUrl postId = buildApiUri $ "post.info?postId=" <> postId

data Response = Response
  { images :: [PostImage]
  , title :: Text
  , creatorName :: Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON Response where
  parseJSON = withObject "Response" $ \outer -> do
    metaInfo <- outer .: "body"
    userInfo <- metaInfo .: "user"
    inner <- metaInfo .: "body"
    Response
      <$> inner .: "images"
      <*> metaInfo .: "title"
      <*> userInfo .: "name"

get :: Text -> Config -> IO Response
get postId config = runReq defaultHttpConfig $ do
  uri <- apiUrl postId
  let (url, uriOptions) = fromJust (useHttpsURI uri)
  let options = basicHeaders config <> uriOptions
  resp <- req GET url NoReqBody jsonResponse options
  return $ responseBody resp
