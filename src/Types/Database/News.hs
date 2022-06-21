module Types.Database.News where

import Types.User
import Types.Picture
import Types.Category
import qualified Types.Database.Category as DBType
import Data.Time.Clock
import qualified Data.Text as T
import Data.Aeson
import Data.Aeson.Types
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow

data NewsList = NewsList [News]

data News = News
    { id :: Integer,
      title :: T.Text,
      createDate :: UTCTime,
      creatorID :: Integer,
      categoryID :: Integer,
      textContent :: T.Text,
      isPublished :: Bool
    }

instance FromRow News where
    fromRow = do
        id <- field
        title <- field
        createDate <- field
        creatorID <- field
        categoryID <- field
        textContent <- field
        isPublished <- field
        return News {..}
