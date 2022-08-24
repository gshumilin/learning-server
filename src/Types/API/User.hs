module Types.API.User where

import Hash (passHashT)
import Data.Time (Day)
import qualified Data.Text as T
import Data.Aeson
import Data.Aeson.Types

data CreateUserRequest = CreateUserRequest
  { reqName :: T.Text,
    reqLogin :: T.Text,
    reqPassword :: T.Text,
    reqIsAdmin :: Bool,
    reqIsAbleToCreateNews :: Bool
  } deriving Show

instance FromJSON CreateUserRequest where
    parseJSON (Object inputJSON) = do
        reqName <- inputJSON .: "name"
        reqLogin <- inputJSON .: "login"
        reqPasswordUnhashed <- inputJSON .: "password"
        let reqPassword = passHashT reqPasswordUnhashed 
        reqIsAdmin <- inputJSON .: "isAdmin"
        reqIsAbleToCreateNews <- inputJSON .: "isAbleToCreateNews"
        return $ CreateUserRequest {..}