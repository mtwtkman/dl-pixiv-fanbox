{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module PixivFanbox.Api.Bell.CountUnread where

import Data.Aeson (FromJSON (..), withObject, (.:))
import GHC.Generics (Generic)
import PixivFanbox.Api (ApiResponse, buildApiUri, performGet)
import PixivFanbox.Config (Config)
import Text.URI (URI)

apiUrl :: IO URI
apiUrl = buildApiUri "bell.countUnread"

newtype Response = Response
  { count :: Int
  }
  deriving (Show, Eq, Generic)

instance FromJSON Response where
  parseJSON = withObject "Response" $ \o -> do
    body <- o .: "body"
    Response <$> body .: "count"

get :: Config -> IO (ApiResponse Response)
get config = apiUrl >>= performGet config Nothing
