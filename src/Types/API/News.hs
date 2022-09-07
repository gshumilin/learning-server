module Types.API.News where

import Control.Monad (mzero)
import Data.Aeson
import qualified Data.Text as T
import qualified Types.Domain.Picture as Domain (Picture (..))

data CreateNewsRequest = CreateNewsRequest
  { title :: T.Text,
    categoryID :: Int,
    textContent :: T.Text,
    pictures :: Maybe [Domain.Picture]
  }
  deriving (Show)

instance FromJSON CreateNewsRequest where
  parseJSON (Object inputJSON) = do
    title <- inputJSON .: "title"
    categoryID <- inputJSON .: "categoryID"
    textContent <- inputJSON .: "textContent"
    pictures <- inputJSON .:? "pictures"
    pure $ CreateNewsRequest {..}
  parseJSON _ = mzero

data EditNewsRequest = EditNewsRequest
  { newsID :: Int,
    newTitle :: Maybe T.Text,
    newCategoryID :: Maybe Int,
    newTextContent :: Maybe T.Text,
    newPictures :: Maybe [Domain.Picture]
  }
  deriving (Show)

instance FromJSON EditNewsRequest where
  parseJSON (Object inputJSON) = do
    newsID <- inputJSON .: "newsID"
    newTitle <- inputJSON .:? "newTitle"
    newCategoryID <- inputJSON .:? "newCategoryID"
    newTextContent <- inputJSON .:? "newTextContent"
    newPictures <- inputJSON .:? "newPictures"
    pure $ EditNewsRequest {..}
  parseJSON _ = mzero
