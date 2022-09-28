{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Types.DB.User where

import Data.Aeson.Types (ToJSON, object, toJSON, (.=))
import qualified Data.Text as T
import Data.Time.Clock (UTCTime)
import Database.PostgreSQL.Simple.FromRow (FromRow)
import GHC.Generics (Generic)

newtype UsersList = UsersList [User] deriving (Generic, ToJSON, Show)

data User = User
  { userId :: Int,
    name :: T.Text,
    login :: T.Text,
    password :: T.Text,
    createDate :: UTCTime,
    isAdmin :: Bool,
    isAbleToCreateNews :: Bool
  }
  deriving (Show, Generic, FromRow)

instance ToJSON User where
  toJSON User {..} =
    object
      [ "userId" .= userId,
        "name" .= name,
        "login" .= login,
        "createDate" .= createDate,
        "isAdmin" .= isAdmin,
        "isAbleToCreateNews" .= isAbleToCreateNews
      ]
