{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Types.DB.News where

import qualified Data.Text as T
import Data.Time.Clock (UTCTime)
import Database.PostgreSQL.Simple.FromRow (FromRow)
import GHC.Generics (Generic)

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
  { oldTitle :: T.Text,
    oldCategoryId :: Int,
    oldTextContent :: T.Text,
    oldPublishStatus :: Bool
  }
  deriving (Show, Generic, FromRow)
