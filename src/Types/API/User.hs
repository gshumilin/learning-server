module Types.API.User where

import Control.Monad (mzero)
import Data.Aeson.Types (FromJSON, Value (..), parseJSON, (.:))
import qualified Data.Text as T
import Hash (passHashT)

data CreateUserRequest = CreateUserRequest
  { reqName :: T.Text,
    reqLogin :: T.Text,
    reqPassword :: T.Text,
    reqIsAdmin :: Bool,
    reqIsAbleToCreateNews :: Bool
  }
  deriving (Show)

instance FromJSON CreateUserRequest where
  parseJSON (Object inputJSON) = do
    reqName <- inputJSON .: "name"
    reqLogin <- inputJSON .: "login"
    reqPasswordUnhashed <- inputJSON .: "password"
    let reqPassword = passHashT reqPasswordUnhashed
    reqIsAdmin <- inputJSON .: "isAdmin"
    reqIsAbleToCreateNews <- inputJSON .: "isAbleToCreateNews"
    pure $ CreateUserRequest {..}
  parseJSON _ = mzero
