module Types.Category where

import qualified Data.Text as T
import Data.Aeson
import Data.Aeson.Types
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField

type CatName = T.Text
data Category = Empty | CatName (Category) deriving Show

instance FromJSON Category where
    parseJSON (Object inputJSON) = undefined

instance ToJSON Category where
    toJSON cat = undefined

instance FromRow Category where
    fromRow = undefined

instance ToField Category where
    toField = undefined