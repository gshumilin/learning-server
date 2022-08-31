module Types.API.Category where

import Data.Aeson
import qualified Data.Text as T

data CreateCategoryRequest = CreateCategoryRequest
  { title :: T.Text,
    parentCategoryID :: Maybe Int
  }

instance FromJSON CreateCategoryRequest where
  parseJSON (Object inputJSON) = do
    title <- inputJSON .: "title"
    parentCategoryID <- inputJSON .:? "parentCategoryID"
    pure CreateCategoryRequest {..}

data EditCategoryRequest = EditCategoryRequest
  { processedCategoryID :: Int,
    newTitle :: Maybe T.Text,
    newParentCategoryID :: Maybe Int
  }

instance FromJSON EditCategoryRequest where
  parseJSON (Object inputJSON) = do
    processedCategoryID <- inputJSON .: "categoryID"
    newTitle <- inputJSON .:? "newTitle"
    newParentCategoryID <- inputJSON .:? "newParentCategoryID"
    pure EditCategoryRequest {..}
