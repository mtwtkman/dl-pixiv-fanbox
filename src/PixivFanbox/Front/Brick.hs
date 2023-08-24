module PixivFanbox.Front.Brick where

import PixivFanbox.Api.Entity (SupportingCreator)
import PixivFanbox.Config (Config (..))
import qualified PixivFanbox.Config as C
import qualified PixivFanbox.Front.Brick.Phase.SessionId as SessionIdPhase
import qualified PixivFanbox.Front.Brick.Phase.SupportingCreators as SupporingCreatorsPhase
import qualified PixivFanbox.Front.Brick.Phase.Menu as MenuPhase

data State
  = LoggedIn
      { getConfig :: Config,
        getSupportingCreators :: [SupportingCreator],
        getSelectedSupportingCreator :: Maybe SupportingCreator
      }
  | NotLoggedIn
  deriving (Show, Eq)

selectSupporitngCreator :: State -> SupportingCreator -> State
selectSupporitngCreator s c = LoggedIn (getConfig s) (getSupportingCreators s) (Just c)

initialState :: Maybe Config -> State
initialState (Just config) = LoggedIn config [] Nothing
initialState Nothing = NotLoggedIn

isLoggedIn :: State -> Bool
isLoggedIn state = state /= NotLoggedIn

login :: Int -> IO State
login defaultDownloadChunkSize = do
  sessionId <- SessionIdPhase.runApp
  let config = Config sessionId defaultDownloadChunkSize
  return $ initialState (Just config)

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

runApp :: Int -> FilePath -> IO ()
runApp defaultDownloadChunkSize confPath = do
  conf <- findConf defaultDownloadChunkSize confPath
  loggedInState <- case initialState conf of
    NotLoggedIn -> do
      login defaultDownloadChunkSize
    loggedIn -> return loggedIn
  selected <- MenuPhase.runApp
  putStrLn $ "state: " <> show loggedInState <> " selected: " <> show selected
  -- creator <- manipulateCreatorList loggedInState
  -- putStrLn $ "creator: " <> show creator <> " state: " <> show loggedInState
