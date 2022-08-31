module Types.Database.News where

import Data.Aeson
import Data.Aeson.Types
import qualified Data.Text as T
import Data.Time.Clock
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import qualified Types.Database.Category as DBType
import Types.Domain.Category
import Types.Domain.Picture
import Types.Domain.User

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
