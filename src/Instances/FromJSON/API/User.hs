{-# LANGUAGE OverloadedStrings , RecordWildCards #-}

module Instances.FromJSON.API.User where

import Data.Aeson
import Data.Aeson.Types
import Types.User
import Types.API.User

instance FromJSON CreateUserRequest where
    parseJSON (Object inputJSON) = do
        name <- inputJSON .: "name"
        login <- inputJSON .: "login"
        password <- inputJSON .: "password"
        return $ CreateUserRequest {..}