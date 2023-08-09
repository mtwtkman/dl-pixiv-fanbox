{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module PixivFanbox.Api.SupportingCreatorList where

import Data.Aeson (FromJSON(..), withObject, (.:))
import Data.Maybe (fromJust)
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
import PixivFanbox.Api
  ( basicHeaders,
    buildApiUri,
  )
import PixivFanbox.Api.Entity (SupportingCreator)
import PixivFanbox.Config (Config (..))
import Text.URI (URI)

apiUrl :: Req URI
apiUrl = buildApiUri "plan.listSupporting"

newtype Response = Response
  { creators :: [SupportingCreator]
  }
  deriving (Show, Eq, Generic)

instance FromJSON Response where
  parseJSON = withObject "Response" $ \o -> do
    Response <$> o .: "body"

get :: Config -> IO Response
get config = runReq defaultHttpConfig $ do
  uri <- apiUrl
  let url = fst $ fromJust (useHttpsURI uri)
  let options = basicHeaders config
  resp <- req GET url NoReqBody jsonResponse options
  return $ responseBody resp
