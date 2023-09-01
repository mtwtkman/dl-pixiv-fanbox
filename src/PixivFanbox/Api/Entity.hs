{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module PixivFanbox.Api.Entity where

import Data.Aeson
  ( FromJSON (..),
    Options (fieldLabelModifier),
    defaultOptions,
    genericParseJSON,
  )
import Data.Char (toLower)
import Data.Text.Lazy (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)

startsWith :: String -> String -> Bool
startsWith [] [] = True
startsWith _ [] = False
startsWith [] _ = False
startsWith (a : as) (b : bs) = (a == b) && startsWith as bs

toLowerHead :: String -> String
toLowerHead (c : xs) = [toLower c] <> xs
toLowerHead [] = ""

dropPrefixOnly :: [String] -> String -> String -> String
dropPrefixOnly _ [] xs = toLowerHead xs
dropPrefixOnly _ _ "" = ""
dropPrefixOnly exclusion (p : ps) raw@(x : xs)
  | raw `elem` exclusion = raw
  | otherwise = if p == x then dropPrefix ps xs else dropPrefix [] raw

dropPrefix :: String -> String -> String
dropPrefix = dropPrefixOnly []

data User = User
  { userId :: Text,
    userName :: Text,
    userIconUrl :: Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON User where
  parseJSON = genericParseJSON customOption
    where
      customOption =
        defaultOptions
          { fieldLabelModifier = dropPrefixOnly ["userId"] "user"
          }

data SupportingCreator = SupportingCreator
  { supportingCreatorId :: Text,
    supportingCreatorTitle :: Text,
    supportingCreatorFee :: Int,
    supportingCreatorDescription :: Text,
    supportingCreatorCoverImageUrl :: Maybe Text,
    supportingCreatorUser :: User,
    supportingCreatorCreatorId :: Text,
    supportingCreatorHasAdultContent :: Bool,
    supportingCreatorPaymentMethod :: Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON SupportingCreator where
  parseJSON = genericParseJSON customOption
    where
      customOption =
        defaultOptions
          { fieldLabelModifier = dropPrefix "supportingCreator"
          }

data PostItem = PostItem
  { postItemId :: Text,
    postItemTitle :: Text,
    postItemFeeRequired :: Int,
    postItemPublishedDatetime :: UTCTime,
    postItemUpdatedDatetime :: UTCTime,
    postItemTags :: [Text],
    postItemIsLiked :: Bool,
    postItemLikeCount :: Int,
    postItemCommentCount :: Int,
    postItemIsRestricted :: Bool,
    postItemUser :: User,
    postItemCreatorId :: Text,
    postItemHasAdultContent :: Bool,
    postItemCover :: Cover,
    postItemExcerpt :: Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON PostItem where
  parseJSON = genericParseJSON customOption
    where
      customOption =
        defaultOptions
          { fieldLabelModifier = dropPrefix "postItem"
          }

data Cover = Cover
  { coverType :: Text,
    coverUrl :: Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON Cover where
  parseJSON = genericParseJSON customOption
    where
      customOption =
        defaultOptions
          { fieldLabelModifier = dropPrefix "cover"
          }

data PostImage = PostImage
  { postImageId :: Text,
    postImageExtension :: Text,
    postImageWidth :: Int,
    postImageHeight :: Int,
    postImageOriginalUrl :: Text,
    postImageThumbnailUrl :: Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON PostImage where
  parseJSON = genericParseJSON customOption
    where
      customOption =
        defaultOptions
          { fieldLabelModifier = dropPrefix "postImage"
          }

data FoundCreator = FoundCreator
  { foundCreatorCreatorId :: Text,
    foundCreatorDescription :: Text,
    foundCreatorHasAdultContent :: Bool,
    foundCreatorHasBoothShop :: Bool,
    foundCreatorIsAcceptingRequest :: Bool,
    foundCreatorIsFollowed :: Bool,
    foundCreatorIsStopped :: Bool,
    foundCreatorIsSupported :: Bool,
    foundCreatorUser :: User
  }
  deriving (Show, Eq, Generic)

instance FromJSON FoundCreator where
  parseJSON = genericParseJSON customOption
    where
      customOption =
        defaultOptions
          { fieldLabelModifier = dropPrefix "foundCreator"
          }

data RecommendedCreator = RecommendedCreator
  { recommendedCreatorCreatorId :: Text,
    recommendedCreatorCoverImageUrl :: Maybe Text,
    recommendedCreatorDescription :: Text,
    recommendedCreatorHasAdultContent :: Bool,
    recommendedCreatorHasBoothShop :: Bool,
    recommendedCreatorIsAcceptingRequest :: Bool,
    recommendedCreatorIsFollowed :: Bool,
    recommendedCreatorIsStopped :: Bool,
    recommendedCreatorIsSupported :: Bool
  }
  deriving (Show, Eq, Generic)

instance FromJSON RecommendedCreator where
  parseJSON = genericParseJSON customOption
    where
      customOption =
        defaultOptions
          { fieldLabelModifier = dropPrefix "recommendedCreator"
          }
