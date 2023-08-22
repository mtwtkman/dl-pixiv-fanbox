module PixivFanbox.Front.Brick.State where

import Data.ByteString.Char8 (ByteString)
import PixivFanbox.Front.Brick.Phase (Phase)

data State = State
  { _phase :: Phase,
    _sessionId :: Maybe ByteString
  }
  deriving (Show)
