{-# LANGUAGE DeriveGeneric #-}

module Types.Domain.User where

import Data.Aeson.Types (FromJSON, ToJSON, object, toJSON, (.=))
import qualified Data.Text as T
import Data.Time (UTCTime)
import Database.PostgreSQL.Simple.FromRow (FromRow, field, fromRow)
import GHC.Generics (Generic)

newtype UsersList = UsersList [User] deriving (Generic, Show)

instance ToJSON UsersList

data User = User
  { name :: T.Text,
    login :: T.Text,
    password :: T.Text,
    createDate :: UTCTime,
    isAdmin :: Bool,
    isAbleToCreateNews :: Bool
  }
  deriving (Generic, Show)

instance FromJSON User

instance ToJSON User where
  toJSON User {..} =
    object
      [ "name" .= name,
        "login" .= login,
        --"password" .= password,
        "createDate" .= createDate,
        "isAdmin" .= isAdmin,
        "isAbleToCreateNews" .= isAbleToCreateNews
      ]

instance FromRow User where
  fromRow = User <$> field <*> field <*> field <*> field <*> field <*> field
