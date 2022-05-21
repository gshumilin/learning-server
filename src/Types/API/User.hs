module Types.API.User where

import Data.Time (Day)
import qualified Data.Text as T

data CreateUserRequest = CreateUserRequest
  { reqName :: T.Text,
    reqLogin :: T.Text,
    reqPassword :: T.Text
  } deriving Show