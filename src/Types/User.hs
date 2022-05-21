module Types.User where

import Data.Time (Day)
import qualified Data.Text as T

data UsersList = UsersList [User]

data User = User
  { name :: T.Text,
    login :: T.Text,
    password :: T.Text,
    createDate :: Day,
    isAdmin :: Bool,
    isAbleToCreateNews :: Bool
  } deriving Show