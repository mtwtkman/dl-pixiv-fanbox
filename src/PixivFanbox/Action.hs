module PixivFanbox.Action where

import Data.Aeson (FromJSON)
import PixivFanbox.Api (ApiResponse)
import Data.Functor ((<&>))

respMap :: FromJSON m => b -> (m -> b) -> IO (ApiResponse m) -> IO b
respMap defaultValue f resp = resp <&> either (const defaultValue) f
