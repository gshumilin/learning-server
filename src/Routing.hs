module Routing where

import Types.User
import qualified Endpoints.User
import Network.Wai
import qualified Data.ByteString.Char8 as BS

application :: Application
application req respond 
    | reqPath == "/getUsersList" = respond =<< Endpoints.User.getUsersList
    | reqPath == "/createUser" = respond =<< Endpoints.User.createUser req
    | otherwise = undefined
    where
        reqPath = rawPathInfo req
        reqQuery = rawQueryString req

