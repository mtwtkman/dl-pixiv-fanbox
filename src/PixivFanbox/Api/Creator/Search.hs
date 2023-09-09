{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module PixivFanbox.Api.Creator.Search where

import Data.Aeson (FromJSON (..), withObject, (.:))
import Data.Text.Lazy (Text)
import GHC.Generics (Generic)
import Network.HTTP.Req (Option, Scheme (Https), (=:))
import PixivFanbox.Api (ApiResponse, buildApiUri, performGet)
import PixivFanbox.Api.Entity (FoundCreator)
import PixivFanbox.Config (Config)
import Text.URI (URI)

apiUrl :: IO URI
apiUrl = buildApiUri "creator.search"

query :: Text -> Int -> Option 'Https
query q page = "q" =: q <> "page" =: page

newtype Response = Response
  { creators :: [FoundCreator]
  }
  deriving (Show, Eq, Generic)

instance FromJSON Response where
  parseJSON = withObject "Response" $ \o -> do
    inner <- o .: "body"
    Response <$> inner .: "creators"

get :: Text -> Int -> Config -> IO (ApiResponse Response)
get q page config = apiUrl >>= performGet config (Just $ query q page)
