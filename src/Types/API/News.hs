module Types.API.News where

import Types.Domain.Picture
import qualified Data.Text as T
import Data.Aeson
import Data.Aeson.Types
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField

data CreateNewsRequest = CreateNewsRequest
    { title :: T.Text,
      categoryID :: Int,
      textContent :: T.Text,
      picturesArray :: Maybe [Picture]
    } deriving Show

instance FromJSON CreateNewsRequest where
    parseJSON (Object inputJSON) = do
        title <- inputJSON .: "title"
        categoryID <- inputJSON .: "categoryID"
        textContent <- inputJSON .: "textContent"
        picturesArray <- inputJSON .:? "picturesArray"
        return $ CreateNewsRequest {..}



data EditNewsRequest = EditNewsRequest
    { newsID :: Int,
      newTitle :: Maybe T.Text,
      newCategoryID :: Maybe Int,
      newTextContent :: Maybe T.Text,
      newPicturesArray :: Maybe [Picture]
    } deriving Show

instance FromJSON EditNewsRequest where
    parseJSON (Object inputJSON) = do
        newsID <- inputJSON .: "newsID"
        newTitle <- inputJSON .:? "newTitle"
        newCategoryID <- inputJSON .:? "newCategoryID"
        newTextContent <- inputJSON .:? "newTextContent"
        newPicturesArray <- inputJSON .:? "newPicturesArray"
        return $ EditNewsRequest {..}



data GetNewsRequest = GetNewsRequest
    { sortBy :: Maybe SortBy
    }

data SortBy = NewsCreationDate | NewsCreator | NewsCategory | NewsNumbersOfPictures