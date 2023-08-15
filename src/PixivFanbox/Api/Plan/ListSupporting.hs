{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module PixivFanbox.Api.Plan.ListSupporting where

import Data.Aeson (FromJSON (..), withObject, (.:))
import GHC.Generics (Generic)
import PixivFanbox.Api
  ( ApiResponse,
    buildApiUri,
    performGet,
  )
import PixivFanbox.Api.Entity (SupportingCreator)
import PixivFanbox.Config (Config (..))
import Text.URI (URI)

apiUrl :: IO URI
apiUrl = buildApiUri "plan.listSupporting"

newtype Response = Response
  { creators :: [SupportingCreator]
  }
  deriving (Show, Eq, Generic)

instance FromJSON Response where
  parseJSON = withObject "Response" $ \o -> do
    Response <$> o .: "body"

get :: Config -> IO (ApiResponse Response)
get config = apiUrl >>= performGet config
