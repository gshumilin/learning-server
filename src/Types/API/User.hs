{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Types.API.User where

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
        reqPassword <- inputJSON .: "password"
        reqIsAdmin <- inputJSON .: "isAdmin"
        reqIsAbleToCreateNews <- inputJSON .: "isAbleToCreateNews"
        return $ CreateUserRequest {..}