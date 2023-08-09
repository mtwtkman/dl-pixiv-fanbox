{-# LANGUAGE OverloadedStrings #-}

module PixivFanbox.Config where

import qualified Data.ByteString.Lazy.Char8 as B
import System.Directory (doesFileExist)
import System.Environment (lookupEnv)
import Text.Regex.TDFA ((=~))

newtype Config = Config B.ByteString
  deriving (Show)

data Error
  = NoEnvVarProvided B.ByteString
  | ConfigFileNotFound
  | SessionIdNotFound
  deriving (Show)

envVar :: B.ByteString
envVar = "DL_PIXIV_FANBOX_SESSION_ID"

fromEnv :: IO (Either Error Config)
fromEnv = do
  val <- lookupEnv (B.unpack envVar)
  let result = case val of
        Just sessionId -> Right (Config (B.pack sessionId))
        Nothing -> Left (NoEnvVarProvided ("`" <> envVar <> "` must be provided."))
  return result

fromConfigFile :: FilePath -> IO (Either Error Config)
fromConfigFile configFilePath = do
  isFile <- doesFileExist configFilePath
  if isFile
    then do
      content <- B.readFile configFilePath
      return $ fmap Config (parseContent content)
    else return (Left ConfigFileNotFound)
  where
    sessionIdPattern :: B.ByteString
    sessionIdPattern = "session_id=([a-zA-Z0-9_]+)"

    parseContent :: B.ByteString -> Either Error B.ByteString
    parseContent content = do
      let (_, _, _, matched) = content =~ sessionIdPattern :: (B.ByteString, B.ByteString, B.ByteString, [B.ByteString])
      case matched of
        [x] -> Right x
        _ -> Left SessionIdNotFound
