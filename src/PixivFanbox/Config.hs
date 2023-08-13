{-# LANGUAGE OverloadedStrings #-}

module PixivFanbox.Config where

import qualified Data.ByteString.Lazy.Char8 as B
import System.Directory (doesFileExist)
import System.Environment (lookupEnv)
import Text.Regex.TDFA ((=~))

data Config = Config
  { configSessionId :: B.ByteString,
    configDownloadChunkSize :: Int
  }
  deriving (Show)

data Error
  = NoEnvVarProvided B.ByteString
  | ConfigFileNotFound
  | SessionIdNotFound
  | ZeroDownloadChunkSize
  deriving (Show)

sessionIdEnvVar :: B.ByteString
sessionIdEnvVar = "DL_PIXIV_FANBOX_SESSION_ID"

downloadChunkSizeEnvVar :: B.ByteString
downloadChunkSizeEnvVar = "DL_PIXIV_FANBOX_DOWNLOAD_CHUNK_SIZE"

fromEnv :: Int -> IO (Either Error Config)
fromEnv defaultDownloadChunkSize = do
  sessionIdVal <- lookupEnv (B.unpack sessionIdEnvVar)
  downloadChunkSizeVal <- lookupEnv (B.unpack downloadChunkSizeEnvVar)

  let result = case (sessionIdVal, downloadChunkSizeVal) of
        (Just sessionId, downloadChunkSize) -> Right (Config (B.pack sessionId) (maybe defaultDownloadChunkSize read downloadChunkSize))
        (Nothing, _) -> Left (NoEnvVarProvided ("`" <> sessionIdEnvVar <> "` must be provided."))
  return result

fromConfigFile :: FilePath -> Int -> IO (Either Error Config)
fromConfigFile configFilePath defaultDownloadChunkSize = do
  isFile <- doesFileExist configFilePath
  if isFile
    then do
      content <- B.readFile configFilePath
      return
        ( do
            sessionId <- parseSessionId content
            downloadChunkSize <- parseDownloadChunkSize content
            return $ Config sessionId downloadChunkSize
        )
    else return (Left ConfigFileNotFound)
  where
    sessionIdPattern :: B.ByteString
    sessionIdPattern = "^session_id=([a-zA-Z0-9_]+)"

    downloadChunkSizePattern :: B.ByteString
    downloadChunkSizePattern = "^download_chunk_size=([0-9]+)"

    parseSessionId :: B.ByteString -> Either Error B.ByteString
    parseSessionId content = do
      let (_, _, _, matched) = content =~ sessionIdPattern :: (B.ByteString, B.ByteString, B.ByteString, [B.ByteString])
      case matched of
        [x] -> Right x
        _ -> Left SessionIdNotFound

    parseDownloadChunkSize :: B.ByteString -> Either Error Int
    parseDownloadChunkSize content =
      let (_, _, _, matched) = content =~ downloadChunkSizePattern :: (B.ByteString, B.ByteString, B.ByteString, [B.ByteString])
       in case matched of
            ["0"] -> Left ZeroDownloadChunkSize
            [x] -> Right $ read (B.unpack x)
            _ -> Right defaultDownloadChunkSize
