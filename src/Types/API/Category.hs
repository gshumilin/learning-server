{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Types.API.Category where

import Data.Aeson (FromJSON)
import qualified Data.Text as T
import GHC.Generics (Generic)

data CreateCategoryRequest = CreateCategoryRequest
  { title :: T.Text,
    parentCategoryId :: Maybe Int
  }
  deriving (Generic, Show, FromJSON)

data EditCategoryRequest = EditCategoryRequest
  { processedCategoryId :: Int,
    newTitle :: Maybe T.Text,
    newParentCategoryId :: Maybe Int
  }
  deriving (Generic, Show, FromJSON)
