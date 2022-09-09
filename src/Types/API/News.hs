module Types.API.News where

import Control.Monad (mzero)
import Data.Aeson
import qualified Data.Text as T
import qualified Types.Domain.Picture as Domain (Picture (..))

data CreateNewsRequest = CreateNewsRequest
  { title :: T.Text,
    categoryId :: Int,
    textContent :: T.Text,
    pictures :: Maybe [Domain.Picture]
  }
  deriving (Show)

instance FromJSON CreateNewsRequest where
  parseJSON (Object inputJSON) = do
    title <- inputJSON .: "title"
    categoryId <- inputJSON .: "categoryId"
    textContent <- inputJSON .: "textContent"
    pictures <- inputJSON .:? "pictures"
    pure $ CreateNewsRequest {..}
  parseJSON _ = mzero

data EditNewsRequest = EditNewsRequest
  { newsId :: Int,
    newTitle :: Maybe T.Text,
    newCategoryId :: Maybe Int,
    newTextContent :: Maybe T.Text,
    newPictures :: Maybe [Domain.Picture]
  }
  deriving (Show)

instance FromJSON EditNewsRequest where
  parseJSON (Object inputJSON) = do
    newsId <- inputJSON .: "newsId"
    newTitle <- inputJSON .:? "newTitle"
    newCategoryId <- inputJSON .:? "newCategoryId"
    newTextContent <- inputJSON .:? "newTextContent"
    newPictures <- inputJSON .:? "newPictures"
    pure $ EditNewsRequest {..}
  parseJSON _ = mzero
