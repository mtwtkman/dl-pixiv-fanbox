module PixivFanbox.Action.RetrieveContents where

import qualified Data.Text as Tx
import qualified Data.Text.Lazy as LT
import PixivFanbox.Action (respMap)
import PixivFanbox.Api.Post.Info (Response (images), get)
import PixivFanbox.Config (Config)
import PixivFanbox.Downloader (DestInfo, retrieveImageChunk)

perform :: Config -> DestInfo -> LT.Text -> IO [Tx.Text]
perform config destInfo postId = respMap [] images (get postId config) >>= retrieveImageChunk config destInfo
