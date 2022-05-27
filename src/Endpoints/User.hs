{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Endpoints.User where

import Types.User
import Types.API.User
import Instances.ToJSON.User
import Instances.FromJSON.User
import Instances.FromJSON.API.User
import DataBaseQueries (parseUsersList)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Char8 as BS
import Data.Text.Encoding (encodeUtf8)
import Data.Time
import Data.Time.Calendar
import Data.Time.Clock
import Network.HTTP.Types (hContentType, status200, status400)
import Network.Wai
import Data.Aeson
import Data.Aeson.Encode.Pretty (encodePretty)

getUsersList :: IO (Response)
getUsersList = do 
    usersList <- parseUsersList
    let jsonUsersList = encodePretty usersList
    return $ responseLBS status200 [(hContentType, "text/plain")] $ jsonUsersList

createUser :: Request -> IO (Response)
createUser req = do
    rawJSON <- getRequestBodyChunk req
    let req = decodeStrict rawJSON :: Maybe CreateUserRequest
    case req of
        Nothing -> do 
            putStrLn "Invalid JSON"
            return $ responseLBS status400 [(hContentType, "text/plain")] $ "Bad Request: Invalid JSON\n"
        Just createUserReq -> do
            putStrLn . show $ rawJSON
            newUser <- makingUser createUserReq
            addUser (newUser)
            return $ responseLBS status200 [(hContentType, "text/plain")] $ "all done"

addUser :: User -> IO ()        --позже написать функцию добавления пользователя
addUser x = putStrLn $ show x

makingUser :: CreateUserRequest -> IO (User)
makingUser CreateUserRequest {..} = do
    currTime <- getCurrentTime
    return $ 
        User { name = reqName,
               login = reqLogin,
               password = reqPassword,
               createDate = currTime,
               isAdmin = reqIsAdmin,
               isAbleToCreateNews = reqIsAbleToCreateNews
             }
