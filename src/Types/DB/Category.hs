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
  { categoryID :: Int,
    title :: T.Text,
    parentID :: Maybe Int
  }
  deriving (Show)

instance ToJSON Category where
  toJSON Category {..} =
    object
      [ "categoryID" .= categoryID,
        "title" .= title,
        "parentID" .= parentID
      ]

instance FromRow Category where
  fromRow = do
    categoryID <- field
    title <- field
    parentID <- field
    pure Category {..}
