module Types.Database.Category where

import qualified Data.Text as T
import Data.Aeson
import Data.Aeson.Types
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField

data Category = Category
    { categoryID :: Int,
      title :: T.Text,
      parentID :: Maybe Int
    } deriving Show

instance FromRow Category where
    fromRow = do
        categoryID <- field
        title <- field
        parentID <- field
        pure Category {..}