module Types.DB.User where

import Data.Aeson.Types (ToJSON, object, toJSON, (.=))
import qualified Data.Text as T
import Data.Time.Clock (UTCTime)
import Database.PostgreSQL.Simple.FromRow (FromRow, field, fromRow)

newtype UsersList = UsersList [User]

instance ToJSON UsersList where
  toJSON (UsersList list) =
    object
      [ "usersList" .= list
      ]

data User = User
  { userID :: Int,
    name :: T.Text,
    login :: T.Text,
    password :: T.Text,
    createDate :: UTCTime,
    isAdmin :: Bool,
    isAbleToCreateNews :: Bool
  }
  deriving (Show)

instance ToJSON User where
  toJSON User {..} =
    object
      [ "userID" .= userID,
        "name" .= name,
        "login" .= login,
        --, "password" .= password
        "createDate" .= createDate,
        "isAdmin" .= isAdmin,
        "isAbleToCreateNews" .= isAbleToCreateNews
      ]

instance FromRow User where
  fromRow = User <$> field <*> field <*> field <*> field <*> field <*> field <*> field
