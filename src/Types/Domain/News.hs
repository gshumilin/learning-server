{-# LANGUAGE DeriveGeneric #-}

module Types.Domain.News where

import Data.Aeson.Types (FromJSON, ToJSON)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime)
import GHC.Generics (Generic)
import qualified Types.Domain.Category as Domain (Category (..))
import qualified Types.Domain.User as Domain (User (..))

newtype NewsList = NewsList [News] deriving (Generic, Show)

instance ToJSON NewsList

data News = News
  { newsId :: Int,
    title :: T.Text,
    createDate :: UTCTime,
    creator :: Domain.User,
    category :: Maybe Domain.Category,
    textContent :: T.Text,
    pictures :: Maybe [T.Text],
    isPublished :: Bool,
    numbersOfPictures :: Int
  }
  deriving (Generic, Show)

instance FromJSON News

instance ToJSON News
