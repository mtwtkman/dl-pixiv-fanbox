module PixivFanbox.Front.Brick.Phase where

import qualified PixivFanbox.Front.Brick.Phase.SessionId as SessionIdPhase

data Phase
  = SpecifySessionId SessionIdPhase.State
  | SearchCreators
  | FetchPosts
  | RetrieveImages
  deriving (Show)
