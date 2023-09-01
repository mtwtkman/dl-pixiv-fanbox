{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module PixivFanbox.Api.Creator.Search where

import Data.Aeson (FromJSON (..), withObject, (.:))
import Data.Text.Lazy (Text, pack)
import GHC.Generics (Generic)
import PixivFanbox.Api (ApiResponse, buildApiUri, performGet)
import PixivFanbox.Api.Entity (FoundCreator)
import PixivFanbox.Config (Config)
import Text.URI (URI)

apiUrl :: Text -> Int -> IO URI
apiUrl query page =
  buildApiUri $
    "search"
      <> "q="
      <> query
      <> "page="
      <> pack (show page)

newtype Response = Response
  { creators :: [FoundCreator]
  }
  deriving (Show, Eq, Generic)

instance FromJSON Response where
  parseJSON = withObject "Response" $ \o -> do
    Response <$> o .: "body"

get :: Text -> Int -> Config -> IO (ApiResponse Response)
get query page config = apiUrl query page >>= performGet config
