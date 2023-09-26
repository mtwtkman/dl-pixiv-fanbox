{-# LANGUAGE OverloadedStrings #-}

module PixivFanbox.Config where

import qualified Data.ByteString.Lazy.Char8 as B
import System.Directory (doesFileExist)
import System.Environment (lookupEnv)
import Text.Regex.TDFA ((=~))

newtype Config = Config
  { configSessionId :: B.ByteString
  }
  deriving (Show, Eq)

configFromString :: String -> Config
configFromString = Config . B.pack

data Error
  = NoEnvVarProvided B.ByteString
  | ConfigFileNotFound
  | SessionIdNotFound
  deriving (Show)

sessionIdEnvVar :: B.ByteString
sessionIdEnvVar = "DL_PIXIV_FANBOX_SESSION_ID"

fromEnv :: IO (Either Error Config)
fromEnv = do
  sessionIdVal <- lookupEnv (B.unpack sessionIdEnvVar)

  let result = case sessionIdVal of
        Just sessionId -> Right (Config (B.pack sessionId))
        Nothing -> Left (NoEnvVarProvided ("`" <> sessionIdEnvVar <> "` must be provided."))
  return result

fromConfigFile :: FilePath -> IO (Either Error Config)
fromConfigFile configFilePath = do
  isFile <- doesFileExist configFilePath
  if isFile
    then do
      content <- B.readFile configFilePath
      return
        ( do
            sessionId <- parseSessionId content
            return $ Config sessionId
        )
    else return (Left ConfigFileNotFound)
  where
    sessionIdPattern :: B.ByteString
    sessionIdPattern = "^session_id=([a-zA-Z0-9_]+)"

    parseSessionId :: B.ByteString -> Either Error B.ByteString
    parseSessionId content = do
      let (_, _, _, matched) = content =~ sessionIdPattern :: (B.ByteString, B.ByteString, B.ByteString, [B.ByteString])
      case matched of
        [x] -> Right x
        _ -> Left SessionIdNotFound
