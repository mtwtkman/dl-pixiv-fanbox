module PixivFanbox.Action.FetchPosts where

import Data.Functor ((<&>))
import Data.Text.Lazy (Text)
import PixivFanbox.Api.Entity (PostItem)
import PixivFanbox.Api.Post.ListCreator (Response (items), get)
import PixivFanbox.Config (Config)

perform :: Int -> Text -> Config -> IO [PostItem]
perform limit creatorId config = get limit creatorId config <&> items
