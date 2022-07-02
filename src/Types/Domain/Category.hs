module Types.Domain.Category where

import qualified Data.Text as T
import Data.Aeson
import Data.Aeson.Types
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField

data Category = Category
    { categoryID :: Int,
      title :: T.Text,
      parent :: Maybe Category
    } deriving Show

instance FromJSON Category where
    parseJSON (Object inputJSON) = undefined

instance ToJSON Category where
    toJSON Category {..} = do
        object [ "categoryID" .= categoryID
               , "title" .= title
               , "parent" .= parent
               ]

instance ToField Category where
    toField = undefined