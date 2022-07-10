module Types.API.Category where

import qualified Data.Text as T
import Data.Aeson

data CreateCategoryRequest = CreateCategoryRequest
    {   title :: T.Text,
        parentCategoryID :: Maybe Int
    }

instance FromJSON CreateCategoryRequest where
    parseJSON (Object inputJSON) = do
        title <- inputJSON .: "title"
        parentCategoryID <- inputJSON .:? "parentCategoryID"
        return CreateCategoryRequest {..}

data EditCategoryRequest = EditCategoryRequest
    {   categoryID :: Int,
        newTitle :: Maybe T.Text,
        newParentCategoryID :: Maybe Int
    }

instance FromJSON EditCategoryRequest where
    parseJSON (Object inputJSON) = do
        categoryID <- inputJSON .: "categoryID"
        newTitle <- inputJSON .:? "newTitle"
        newParentCategoryID <- inputJSON .:? "newParentCategoryID"
        return EditCategoryRequest {..}