{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Types.DB.Category where

import Data.Aeson.Types (ToJSON)
import qualified Data.Text as T
import Database.PostgreSQL.Simple.FromRow (FromRow)
import GHC.Generics (Generic)

newtype Categories = Categories [Category] deriving (Generic, Show, ToJSON)

data Category = Category
  { categoryId :: Int,
    title :: T.Text,
    parentId :: Maybe Int
  }
  deriving (Generic, Show, ToJSON, FromRow)
