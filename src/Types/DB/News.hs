module Types.DB.News where

import qualified Data.Text as T
import Data.Time.Clock (UTCTime)
import Database.PostgreSQL.Simple.FromRow (FromRow, field, fromRow)

newtype NewsList = NewsList [News]

data News = News
  { newsID :: Int,
    title :: T.Text,
    createDate :: UTCTime,
    creatorID :: Int,
    creatorLogin :: T.Text,
    categoryID :: Int,
    categoryTitle :: T.Text,
    textContent :: T.Text,
    isPublished :: Bool,
    numbersOfPictures :: Int
  }
  deriving (Show)

instance FromRow News where
  fromRow = do
    newsID <- field
    title <- field
    createDate <- field
    creatorID <- field
    creatorLogin <- field
    categoryID <- field
    categoryTitle <- field
    textContent <- field
    isPublished <- field
    numbersOfPictures <- field
    pure News {..}

data EditedNewsFields = EditedNewsFields
  { oldCreatorID :: Int,
    oldTitle :: T.Text,
    oldCategoryID :: Int,
    oldTextContent :: T.Text
  }
  deriving (Show)

instance FromRow EditedNewsFields where
  fromRow = do
    oldCreatorID <- field
    oldTitle <- field
    oldCategoryID <- field
    oldTextContent <- field
    pure EditedNewsFields {..}
