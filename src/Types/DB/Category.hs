module Types.DB.Category where

import qualified Data.Text as T
import Database.PostgreSQL.Simple.FromRow (FromRow, field, fromRow)

data Category = Category
  { categoryID :: Int,
    title :: T.Text,
    parentID :: Maybe Int
  }
  deriving (Show)

instance FromRow Category where
  fromRow = do
    categoryID <- field
    title <- field
    parentID <- field
    pure Category {..}
