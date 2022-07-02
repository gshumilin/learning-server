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
      categoryID :: Int,
      textContent :: T.Text,
      isPublished :: Bool
    }

instance FromRow News where
    fromRow = do
        newsID <- field
        title <- field
        createDate <- field
        creatorID <- field
        categoryID <- field
        textContent <- field
        isPublished <- field
        return News {..}
