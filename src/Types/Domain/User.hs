module Types.Domain.User where

import Control.Monad (mzero)
import Data.Aeson.Types (FromJSON, ToJSON, Value (..), object, parseJSON, toJSON, (.:), (.=))
import qualified Data.Text as T
import Data.Time (UTCTime)
import Database.PostgreSQL.Simple.FromRow (FromRow, field, fromRow)

newtype UsersList = UsersList [User]

instance ToJSON UsersList where
  toJSON (UsersList list) =
    object
      [ "usersList" .= list
      ]

data User = User
  { name :: T.Text,
    login :: T.Text,
    password :: T.Text,
    createDate :: UTCTime,
    isAdmin :: Bool,
    isAbleToCreateNews :: Bool
  }
  deriving (Show)

instance FromJSON User where
  parseJSON (Object inputJSON) = do
    name <- inputJSON .: "name"
    login <- inputJSON .: "login"
    password <- inputJSON .: "password"
    createDate <- inputJSON .: "createDate"
    isAdmin <- inputJSON .: "isAdmin"
    isAbleToCreateNews <- inputJSON .: "isAbleToCreateNews"
    pure $ User {..}
  parseJSON _ = mzero

instance ToJSON User where
  toJSON User {..} =
    object
      [ "name" .= name,
        "login" .= login,
        --, "password" .= password
        "createDate" .= createDate,
        "isAdmin" .= isAdmin,
        "isAbleToCreateNews" .= isAbleToCreateNews
      ]

instance FromRow User where
  fromRow = User <$> field <*> field <*> field <*> field <*> field <*> field
