module Types.News where

import Types.User
import Types.Picture
import Types.Category
import Data.Time.Clock
import qualified Data.Text as T
import Data.Aeson
import Data.Aeson.Types

data News = News
    { header :: T.Text,
      createDate :: UTCTime,
      creator :: User,
      categogy :: Category T.Text,
      textContent :: T.Text,
      picturesArray :: [Picture],
      isPublished :: Bool
    }

instance FromJSON News where
    parseJSON (Object inputJSON) = do
        header <- inputJSON .: "header"
        createDate <- inputJSON .: "createDate"
        creator <- inputJSON .: "creator"
        categogy <- inputJSON .: "categogy"
        textContent <- inputJSON .: "textContent"
        picturesArray <- inputJSON .: "picturesArray"
        isPublished <- inputJSON .: "isPublished"
        return $ News {..}

instance ToJSON News where
    toJSON News {..} = 
        object [ "header" .= header
               , "createDate" .= createDate
               , "creator" .= creator
               , "categogy" .= categogy
               , "textContent" .= textContent
               , "picturesArray" .= picturesArray
               , "isPublished" .= isPublished
               ]

