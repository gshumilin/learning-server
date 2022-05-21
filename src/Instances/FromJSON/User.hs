{-# LANGUAGE OverloadedStrings , RecordWildCards #-}

module Instances.FromJSON.User where

import Data.Aeson
import Data.Aeson.Types
import Types.User

instance FromJSON User where
    parseJSON (Object inputJSON) = do
        name <- inputJSON .: "name"
        login <- inputJSON .: "login"
        password <- inputJSON .: "password"
        createDate <- inputJSON .: "createDate"
        isAdmin <- inputJSON .: "isAdmin"
        isAbleToCreateNews <- inputJSON .: "isAbleToCreateNews"
        return $ User {..}