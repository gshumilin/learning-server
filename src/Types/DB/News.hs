{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Types.DB.News where

import qualified Data.Text as T
import Data.Time.Clock (UTCTime)
import Database.PostgreSQL.Simple.FromRow (FromRow)
import GHC.Generics (Generic)

newtype NewsList = NewsList [News]

data News = News
  { newsId :: Int,
    title :: T.Text,
    createDate :: UTCTime,
    creatorId :: Int,
    creatorLogin :: T.Text,
    categoryId :: Int,
    categoryTitle :: T.Text,
    textContent :: T.Text,
    isPublished :: Bool,
    numbersOfPictures :: Int
  }
  deriving (Show, FromRow, Generic)

data EditedNewsFields = EditedNewsFields
  { oldCreatorId :: Int,
    oldTitle :: T.Text,
    oldCategoryId :: Int,
    oldTextContent :: T.Text
  }
  deriving (Show, Generic, FromRow)
