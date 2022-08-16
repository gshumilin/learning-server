module Types.Domain.News where

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

instance ToJSON NewsList where
    toJSON (NewsList list) = 
        object  [ "newsList" .= list
                ]

data News = News
    { newsID :: Int,
      title :: T.Text,
      createDate :: UTCTime,
      creator :: User,
      category :: Category,
      textContent :: T.Text,
      picturesLinks :: Maybe [T.Text],
      isPublished :: Bool,
      numbersOfPictures :: Int
    }

instance FromJSON News where
    parseJSON (Object inputJSON) = do
        newsID <- inputJSON .: "newsID"
        title <- inputJSON .: "title"
        createDate <- inputJSON .: "createDate"
        creator <- inputJSON .: "creator"
        category <- inputJSON .: "category"
        textContent <- inputJSON .: "textContent"
        picturesLinks <- inputJSON .: "pictures"
        isPublished <- inputJSON .: "isPublished"
        numbersOfPictures <- inputJSON .: "numbersOfPictures"
        return $ News {..}

instance ToJSON News where
    toJSON News {..} = 
        object [ "newsID" .= newsID
               , "title" .= title
               , "createDate" .= createDate
               , "creator" .= creator
               , "category" .= category
               , "textContent" .= textContent
               , "pictures" .= picturesLinks
               , "isPublished" .= isPublished
               , "numbersOfPictures" .= numbersOfPictures
               ]

