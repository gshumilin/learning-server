module Types.News where

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

instance ToJSON NewsList where
    toJSON (NewsList list) = 
        object  [ "newsList" .= list
                ]

data News = News
    { title :: T.Text,
      createDate :: UTCTime,
      creator :: User,
      category :: Category,
      textContent :: T.Text,
      picturesArray :: PicturesArray,
      isPublished :: Bool
    }

instance FromJSON News where
    parseJSON (Object inputJSON) = do
        title <- inputJSON .: "title"
        createDate <- inputJSON .: "createDate"
        creator <- inputJSON .: "creator"
        category <- inputJSON .: "category"
        textContent <- inputJSON .: "textContent"
        picturesArray <- inputJSON .: "picturesArray"
        isPublished <- inputJSON .: "isPublished"
        return $ News {..}

instance ToJSON News where
    toJSON News {..} = 
        object [ "title" .= title
               , "createDate" .= createDate
               , "creator" .= creator
               , "category" .= category
               , "textContent" .= textContent
               , "picturesArray" .= picturesArray
               , "isPublished" .= isPublished
               ]

