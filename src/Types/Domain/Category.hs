{-# LANGUAGE DeriveGeneric #-}

module Types.Domain.Category where

import Data.Aeson.Types (FromJSON, ToJSON)
import qualified Data.Text as T
import GHC.Generics (Generic)

data Category = Category
  { categoryId :: Int,
    title :: T.Text,
    parent :: Maybe Category
  }
  deriving (Show, Generic)

instance FromJSON Category

instance ToJSON Category
