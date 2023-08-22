module PixivFanbox.Front.Brick.Event where

import qualified PixivFanbox.Front.Brick.Phase.SessionId as SessionIdPhase

data Event =
  SessionIdPhaseEvent SessionIdPhase.Event
  | FetchCreatorsEvent
