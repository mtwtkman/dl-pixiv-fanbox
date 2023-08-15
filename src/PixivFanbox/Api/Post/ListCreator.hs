{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module PixivFanbox.Api.Post.ListCreator where

import Data.Aeson (FromJSON (..), withObject, (.:))
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as Tx
import GHC.Generics (Generic)
import PixivFanbox.Api (ApiResponse, buildApiUri, performGet)
import PixivFanbox.Api.Entity (PostItem)
import PixivFanbox.Config (Config)
import Text.URI (URI)

apiUrl :: Int -> Text -> IO URI
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

get :: Int -> Text -> Config -> IO (ApiResponse Response)
get limit creatorId config = apiUrl limit creatorId >>= performGet config
