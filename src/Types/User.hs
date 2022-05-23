module Types.User where

import Data.Time
import qualified Data.Text as T

data UsersList = UsersList [User]

data User = User
  { name :: T.Text,
    login :: T.Text,
    password :: T.Text,
    createDate :: UTCTime,
    isAdmin :: Bool,
    isAbleToCreateNews :: Bool
  } deriving Show