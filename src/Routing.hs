{-# LANGUAGE OverloadedStrings #-}

module Routing where

import Types.User
import qualified Endpoints.User
import Network.Wai
import qualified Data.ByteString.Char8 as BS

application :: Application
application req respond 
    | reqPath == "getUsersList" = respond $ Endpoints.User.getUsersList
    | otherwise = undefined
    where
        reqPath = BS.tail $ rawPathInfo req

