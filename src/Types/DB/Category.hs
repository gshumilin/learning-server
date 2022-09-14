{-# LANGUAGE DeriveGeneric #-}

module Types.DB.Category where

import Data.Aeson.Types (ToJSON)
import qualified Data.Text as T
import Database.PostgreSQL.Simple.FromRow (FromRow, field, fromRow)
import GHC.Generics (Generic)

newtype Categories = Categories [Category] deriving (Generic, Show)

instance ToJSON Categories

data Category = Category
  { categoryId :: Int,
    title :: T.Text,
    parentId :: Maybe Int
  }
  deriving (Generic, Show)

instance ToJSON Category

instance FromRow Category where
  fromRow = do
    categoryId <- field
    title <- field
    parentId <- field
    pure Category {..}
