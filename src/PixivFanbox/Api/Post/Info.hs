{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module PixivFanbox.Api.Post.Info where

import Data.Aeson (FromJSON (..), withObject, (.:))
import Data.Text.Lazy (Text)
import GHC.Generics (Generic)
import PixivFanbox.Api (ApiResponse, buildApiUri, performGet)
import PixivFanbox.Api.Entity (PostImage)
import PixivFanbox.Config (Config)
import Text.URI (URI)

apiUrl :: Text -> IO URI
apiUrl postId = buildApiUri $ "post.info?postId=" <> postId

data Response = Response
  { images :: [PostImage],
    title :: Text,
    creatorName :: Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON Response where
  parseJSON = withObject "Response" $ \outer -> do
    metaInfo <- outer .: "body"
    userInfo <- metaInfo .: "user"
    inner <- metaInfo .: "body"
    Response
      <$> inner
      .: "images"
      <*> metaInfo
      .: "title"
      <*> userInfo
      .: "name"

get :: Text -> Config -> IO (ApiResponse Response)
get postId config = apiUrl postId >>= performGet config Nothing
