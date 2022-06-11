module Types.News where

import Types.User
import Types.Picture
import Types.Category
import Data.Time.Clock
import qualified Data.Text as T
import Data.Aeson
import Data.Aeson.Types
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow

data News = News
    { header :: T.Text,
      createDate :: UTCTime,
      creator :: User,
      category :: Category T.Text,
      textContent :: T.Text,
      picturesArray :: PicturesArray,
      isPublished :: Bool
    }

instance FromJSON News where
    parseJSON (Object inputJSON) = do
        header <- inputJSON .: "header"
        createDate <- inputJSON .: "createDate"
        creator <- inputJSON .: "creator"
        category <- inputJSON .: "category"
        textContent <- inputJSON .: "textContent"
        picturesArray <- inputJSON .: "picturesArray"
        isPublished <- inputJSON .: "isPublished"
        return $ News {..}

instance ToJSON News where
    toJSON News {..} = 
        object [ "header" .= header
               , "createDate" .= createDate
               , "creator" .= creator
               , "category" .= category
               , "textContent" .= textContent
               , "picturesArray" .= picturesArray
               , "isPublished" .= isPublished
               ]

instance FromRow News where
    fromRow = do
        header <- field
        createDate <- field
        creator <- fromRow
        category <- fromRow
        textContent <- field
        picturesArray <- fromRow
        isPublished <- field
        return News {..}