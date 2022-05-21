{-# LANGUAGE OverloadedStrings , RecordWildCards #-}

module Instances.FromJSON.API.User where

import Data.Aeson
import Data.Aeson.Types
import Types.User
import Types.API.User

instance FromJSON CreateUserRequest where
    parseJSON (Object inputJSON) = do
        reqName <- inputJSON .: "name"
        reqLogin <- inputJSON .: "login"
        reqPassword <- inputJSON .: "password"
        return $ CreateUserRequest {..}