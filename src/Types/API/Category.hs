module Types.API.Category where

import qualified Data.Text as T
import Data.Aeson

data CreateCategoryRequest = Category
    {   title :: T.Text,
        parentCategoryID :: Maybe Int
    }

instance FromJSON CreateCategoryRequest where
    parseJSON (Object inputJSON) = do
        title <- inputJSON .: "title"
        parentCategoryID <- inputJSON .:? "parentCategoryID"
        return Category {..}