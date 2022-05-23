module Types.API.User where

import Data.Time (Day)
import qualified Data.Text as T

data CreateUserRequest = CreateUserRequest
  { reqName :: T.Text,
    reqLogin :: T.Text,
    reqPassword :: T.Text,
    reqIsAdmin :: Bool,
    reqisAbleToCreateNews :: Bool
  } deriving Show