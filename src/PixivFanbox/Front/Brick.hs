module PixivFanbox.Front.Brick where

import Control.Monad (void)
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

appLoop :: MenuPhase.Choice -> State -> IO ()
appLoop _ NotLoggedIn = return ()
appLoop mode (LoggedIn config supportingCreators selectedSupportingCreator) = case mode of
  MenuPhase.Quit -> void (putStrLn "bye.")
  MenuPhase.Setting -> do
    sessionId <- SessionIdPhase.runApp
    let newState = LoggedIn (Config sessionId (configDownloadChunkSize config)) supportingCreators selectedSupportingCreator
    appLoop mode newState
  MenuPhase.SupportingCreatorList -> do
    creator <- SupporingCreatorsPhase.runApp config
    appLoop mode (LoggedIn config supportingCreators (Just creator))

runApp :: Int -> FilePath -> IO ()
runApp defaultDownloadChunkSize confPath = do
  conf <- findConf defaultDownloadChunkSize confPath
  loggedInState <- case initialState conf of
    NotLoggedIn -> do
      login defaultDownloadChunkSize
    loggedIn -> return loggedIn
  selected <- MenuPhase.runApp
  appLoop selected loggedInState
