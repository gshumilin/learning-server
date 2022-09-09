module Types.API.Category where

import Control.Monad (mzero)
import Data.Aeson (FromJSON, Value (..), parseJSON, (.:), (.:?))
import qualified Data.Text as T

data CreateCategoryRequest = CreateCategoryRequest
  { title :: T.Text,
    parentCategoryId :: Maybe Int
  }

instance FromJSON CreateCategoryRequest where
  parseJSON (Object inputJSON) = do
    title <- inputJSON .: "title"
    parentCategoryId <- inputJSON .:? "parentCategoryId"
    pure CreateCategoryRequest {..}
  parseJSON _ = mzero

data EditCategoryRequest = EditCategoryRequest
  { processedCategoryId :: Int,
    newTitle :: Maybe T.Text,
    newParentCategoryId :: Maybe Int
  }

instance FromJSON EditCategoryRequest where
  parseJSON (Object inputJSON) = do
    processedCategoryId <- inputJSON .: "categoryId"
    newTitle <- inputJSON .:? "newTitle"
    newParentCategoryId <- inputJSON .:? "newParentCategoryId"
    pure EditCategoryRequest {..}
  parseJSON _ = mzero
