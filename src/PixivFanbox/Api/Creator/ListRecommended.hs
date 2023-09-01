{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module PixivFanbox.Api.Creator.ListRecommended where

import Data.Aeson (FromJSON (..), withObject, (.:))
import GHC.Generics (Generic)
import PixivFanbox.Api
  ( ApiResponse,
    buildApiUri,
    performGet,
  )
import PixivFanbox.Api.Entity (RecommendedCreator)
import PixivFanbox.Config (Config)
import Text.URI (URI)

apiUrl :: IO URI
apiUrl = buildApiUri "creator.listRecommended"

newtype Response = Response
  { creators :: [RecommendedCreator]
  }
  deriving (Show, Eq, Generic)

instance FromJSON Response where
  parseJSON = withObject "Response" $ \o -> do
    Response <$> o .: "body"

get :: Config -> IO (ApiResponse Response)
get config = apiUrl >>= performGet config
