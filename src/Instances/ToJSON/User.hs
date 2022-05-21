{-# LANGUAGE OverloadedStrings , RecordWildCards #-}

module Instances.ToJSON.User where

import Data.Aeson
import Data.Aeson.Types
import Types.User
import Types.API.User

instance ToJSON UsersList where
    toJSON (UsersList list) = 
        object  [ "usersList" .= list
                ]

instance ToJSON User where
    toJSON User {..} =
        object  [ "name" .= name
                , "login" .= login
                --, "password" .= password
                , "createDate" .= createDate
                , "isAdmin" .= isAdmin
                , "isAbleToCreateNews" .= isAbleToCreateNews
                ] 