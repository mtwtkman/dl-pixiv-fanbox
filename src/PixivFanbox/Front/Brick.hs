module PixivFanbox.Front.Brick where

import qualified Data.Map as M
import PixivFanbox.Api.Entity (PostItem, SupportingCreator)
import PixivFanbox.Config (Config (..))
import qualified PixivFanbox.Front.Brick.Phase.SessionId as SessionIdPhase

data State
  = LoggedIn
      { getConfig :: Config,
        getSupportingCreators :: [SupportingCreator],
        getPosts :: M.Map SupportingCreator [PostItem]
      }
  | NotLoggedIn
  deriving (Show, Eq)

initialState :: Maybe Config -> State
initialState (Just config) = LoggedIn config [] M.empty
initialState Nothing = NotLoggedIn

isLoggedIn :: State -> Bool
isLoggedIn state = state /= NotLoggedIn

login :: Int -> IO State
login defaultDownloadChunkSize = do
  sessionId <- SessionIdPhase.runApp
  let config = Config sessionId defaultDownloadChunkSize
  return $ initialState (Just config)

runApp :: Int -> Maybe Config -> IO ()
runApp defaultDownloadChunkSize maybeConfig = do
  loggedInState <- case initialState maybeConfig of
    NotLoggedIn -> do
      login defaultDownloadChunkSize
    loggedIn -> return loggedIn
  putStrLn $ show loggedInState
