module Types.DB.News where

import qualified Data.Text as T
import Data.Time.Clock (UTCTime)
import Database.PostgreSQL.Simple.FromRow (FromRow, field, fromRow)

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
  deriving (Show)

instance FromRow News where
  fromRow = do
    newsId <- field
    title <- field
    createDate <- field
    creatorId <- field
    creatorLogin <- field
    categoryId <- field
    categoryTitle <- field
    textContent <- field
    isPublished <- field
    numbersOfPictures <- field
    pure News {..}

data EditedNewsFields = EditedNewsFields
  { creatorId :: Int,
    oldTitle :: T.Text,
    oldCategoryId :: Int,
    oldTextContent :: T.Text,
    oldPublishStatus :: Bool
  }
  deriving (Show)

instance FromRow EditedNewsFields where
  fromRow = do
    creatorId <- field
    oldTitle <- field
    oldCategoryId <- field
    oldTextContent <- field
    oldPublishStatus <- field
    pure EditedNewsFields {..}
