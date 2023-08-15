module PixivFanbox.Action.FetchPosts where

import Data.Text.Lazy (Text)
import PixivFanbox.Action (respMap)
import PixivFanbox.Api.Entity (PostItem)
import PixivFanbox.Api.Post.ListCreator (Response (items), get)
import PixivFanbox.Config (Config)

perform :: Int -> Text -> Config -> IO [PostItem]
perform limit creatorId config = respMap [] items (get limit creatorId config)
