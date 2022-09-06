module Types.API.Category where

import Control.Monad (mzero)
import Data.Aeson (FromJSON, Value (..), parseJSON, (.:), (.:?))
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
  parseJSON _ = mzero

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
  parseJSON _ = mzero
