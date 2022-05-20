module Types.User where

import Data.Time.Calendar (Day)
import qualified Data.Text as T

data User = User
  { name :: T.Text,
    login :: T.Text,
    password :: T.Text,
    createDate :: Day,
    isAdmin :: Bool,
    isAbleToCreateNews :: Bool
  }