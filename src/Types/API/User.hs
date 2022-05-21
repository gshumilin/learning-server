module Types.API.User where

import Data.Time (Day)
import qualified Data.Text as T

data CreateUserRequest = CreateUserRequest
  { name :: T.Text,
    login :: T.Text,
    password :: T.Text
  } deriving Show