{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Endpoints.User where

import Types.User
import Types.API.User
import Instances.ToJSON.User
import Instances.FromJSON.User
import Instances.FromJSON.API.User
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Char8 as BS
import Data.Text.Encoding (encodeUtf8)
import Data.Time
import Data.Time.Calendar
import Data.Time.Clock
import Network.HTTP.Types (hContentType, status200)
import Network.Wai
import Data.Aeson
import Data.Aeson.Encode.Pretty (encodePretty)


getUsersList :: Response
getUsersList =
    responseLBS status200 [(hContentType, "text/plain")] $ renderedUsersList
    where
        renderedUsersList = encodePretty usersList

createUser :: Request -> IO (Response)
createUser req = do
    rawJSON <- getRequestBodyChunk req
    let req = decodeStrict rawJSON :: Maybe CreateUserRequest
    case req of
        Nothing -> do 
            putStrLn "Invalid JSON"
            return $ responseLBS status200 [(hContentType, "text/plain")] $ "Invalid JSON\n" --позже исправить респонс
        Just createUserReq -> do
            putStrLn . show $ rawJSON
            newUser <- makingUser createUserReq
            addUser (newUser)
            return $ responseLBS status200 [(hContentType, "text/plain")] $ "user added\n" --позже исправить респонс на json

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
            isAbleToCreateNews = reqisAbleToCreateNews
            }

usersList = UsersList $
            [   User { name = "Gena Shumilin",
                       login = "1000-7_Geneki_7-1000",
                       password = "pleasedonthackme",
                       createDate = UTCTime
                                        { utctDay = ModifiedJulianDay 59719
                                        , utctDayTime = 43200
                                        } ,
                       isAdmin = True,
                       isAbleToCreateNews = True
                     } ,
                User { name = "Oleg Romashin",
                       login = "Colossus_Berutorutoleg",
                       password = "pleasedonthackme2",
                       createDate = UTCTime (ModifiedJulianDay 59719) 43201,
                       isAdmin = False,
                       isAbleToCreateNews = True
                     }
            ]