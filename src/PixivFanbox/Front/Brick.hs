module PixivFanbox.Front.Brick where

import Data.ByteString.Lazy.Char8 (ByteString)
import qualified PixivFanbox.Action.ValidateSessionId as SessionIdValidator
import PixivFanbox.Api.Entity (SupportingCreator)
import PixivFanbox.Config (Config (..))
import qualified PixivFanbox.Config as C
import qualified PixivFanbox.Front.Brick.Phase.Menu as MenuPhase
import qualified PixivFanbox.Front.Brick.Phase.SessionId as SessionIdPhase
import qualified PixivFanbox.Front.Brick.Phase.SupportingCreators as SupporingCreatorsPhase

data State
  = LoggedIn
      { getConfig :: Config,
        getSupportingCreators :: [SupportingCreator],
        getSelectedSupportingCreator :: Maybe SupportingCreator
      }
  | NotLoggedIn Int
  deriving (Show, Eq)

initialLoggedInState :: Config -> State
initialLoggedInState config = LoggedIn config [] Nothing

selectSupporitngCreator :: State -> SupportingCreator -> State
selectSupporitngCreator s c = LoggedIn (getConfig s) (getSupportingCreators s) (Just c)

isLoggedIn :: State -> Bool
isLoggedIn (NotLoggedIn _) = True
isLoggedIn _ = False

manipulateCreatorList :: State -> IO State
manipulateCreatorList state = do
  selected <- SupporingCreatorsPhase.runApp (getConfig state)
  return $ selectSupporitngCreator state selected

findConf :: Int -> FilePath -> IO (Maybe Config)
findConf defaultDownloadChunkSize confPath = do
  envConf <- C.fromEnv defaultDownloadChunkSize
  case envConf of
    Right c -> return (Just c)
    Left _ -> do
      fileConf <- C.fromConfigFile confPath defaultDownloadChunkSize
      case fileConf of
        Right c -> return (Just c)
        _ -> return Nothing

login :: Int -> ByteString -> IO State
login downloadChunkSize sessionId = do
  isValidSessionId <- SessionIdValidator.perform sessionId
  if isValidSessionId
    then return $ initialLoggedInState (Config sessionId downloadChunkSize)
    else SessionIdPhase.runApp >>= login downloadChunkSize

sessionIdValidationLoop :: Int -> ByteString -> IO ()
sessionIdValidationLoop defaultDownloadChunkSize sessionId = do
  isValidSessionId <- SessionIdValidator.perform sessionId
  let state =
        if isValidSessionId
          then initialLoggedInState (Config sessionId defaultDownloadChunkSize)
          else NotLoggedIn defaultDownloadChunkSize
  appLoop MenuPhase.Setting state

appLoop :: MenuPhase.Choice -> State -> IO ()
appLoop MenuPhase.Quit _ = putStrLn "bye"
appLoop chosen@MenuPhase.Setting state = do
  sessionId <- SessionIdPhase.runApp
  let newState = case state of
        LoggedIn config supportingCreators selectedSupportingCreator ->
          LoggedIn (Config sessionId (configDownloadChunkSize config)) supportingCreators selectedSupportingCreator
        NotLoggedIn defaultDownloadChunkSize -> initialLoggedInState (Config sessionId defaultDownloadChunkSize)
  appLoop chosen newState
appLoop chosen@MenuPhase.SupportingCreatorList (LoggedIn config supportingCreators _) = do
  creator <- SupporingCreatorsPhase.runApp config
  appLoop chosen (LoggedIn config supportingCreators (Just creator))
appLoop _ _ = return ()

runApp :: Int -> FilePath -> IO ()
runApp defaultDownloadChunkSize confPath = do
  conf <- findConf defaultDownloadChunkSize confPath
  case conf of
    Nothing -> do
      sessionId <- SessionIdPhase.runApp
      sessionIdValidationLoop defaultDownloadChunkSize sessionId
    Just config -> sessionIdValidationLoop defaultDownloadChunkSize (configSessionId config)
