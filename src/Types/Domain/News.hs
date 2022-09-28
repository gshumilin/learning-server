{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Types.Domain.News where

import Data.Aeson.Types (FromJSON, ToJSON)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime)
import GHC.Generics (Generic)
import qualified Types.Domain.Category as Domain (Category (..))
import qualified Types.Domain.User as Domain (User (..))

newtype NewsList = NewsList [News] deriving (Generic, Show, ToJSON)

data News = News
  { newsId :: Int,
    title :: T.Text,
    createDate :: UTCTime,
    creator :: Domain.User,
    category :: Maybe Domain.Category,
    textContent :: T.Text,
    pictures :: Maybe [T.Text],
    isPublished :: Bool
  }
  deriving (Generic, Show, FromJSON, ToJSON)
