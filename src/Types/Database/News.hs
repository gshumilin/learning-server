module Types.Database.News where

import Types.Domain.User
import Types.Domain.Picture
import Types.Domain.Category
import qualified Types.Database.Category as DBType
import Data.Time.Clock
import qualified Data.Text as T
import Data.Aeson
import Data.Aeson.Types
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow

data NewsList = NewsList [News]

data News = News
    { newsID :: Int,
      title :: T.Text,
      createDate :: UTCTime,
      creatorID :: Int,
      creatorLogin :: T.Text, 
      categoryID :: Int,
      categoryTitle :: T.Text,
      textContent :: T.Text,
      isPublished :: Bool
    } deriving Show

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
      return News {..}
