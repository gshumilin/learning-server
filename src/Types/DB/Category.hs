module Types.DB.Category where

import Data.Aeson.Types (ToJSON, object, toJSON, (.=))
import qualified Data.Text as T
import Database.PostgreSQL.Simple.FromRow (FromRow, field, fromRow)

newtype Categories = Categories [Category]

instance ToJSON Categories where
  toJSON (Categories arr) =
    object
      [ "categories" .= arr
      ]

data Category = Category
  { categoryId :: Int,
    title :: T.Text,
    parentId :: Maybe Int
  }
  deriving (Show)

instance ToJSON Category where
  toJSON Category {..} =
    object
      [ "categoryId" .= categoryId,
        "title" .= title,
        "parentId" .= parentId
      ]

instance FromRow Category where
  fromRow = do
    categoryId <- field
    title <- field
    parentId <- field
    pure Category {..}
