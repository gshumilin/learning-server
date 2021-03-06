module Endpoints.User where

import Types.Domain.User
import Types.Domain.Environment
import Types.API.User
import DatabaseQueries.User (parseUsersList, writeUser)
import Data.Time
import Data.Time.Calendar
import Data.Time.Clock
import Network.HTTP.Types (hContentType, status200, status400)
import Network.Wai
import Data.Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import Control.Monad.Reader

getUsersList :: ReaderT Environment IO (Response)
getUsersList = do
    conn <- asks dbConnection
    usersList <- lift $ parseUsersList conn
    let jsonUsersList = encodePretty (UsersList usersList)
    return $ responseLBS status200 [(hContentType, "text/plain")] $ jsonUsersList

createUser :: Request -> ReaderT Environment IO (Response)
createUser req = do
    conn <- asks dbConnection
    rawJSON <- lift $ getRequestBodyChunk req
    let req = decodeStrict rawJSON :: Maybe CreateUserRequest
    case req of
        Nothing -> do 
            lift $ putStrLn "Invalid JSON"
            lift $ print rawJSON
            return $ responseLBS status400 [(hContentType, "text/plain")] $ "Bad Request: Invalid JSON\n"
        Just createUserReq -> do
            lift . putStrLn . show $ rawJSON
            newUser <- lift $ apiUserTransform createUserReq
            lift $ writeUser conn newUser
            return $ responseLBS status200 [(hContentType, "text/plain")] $ "all done"

apiUserTransform :: CreateUserRequest -> IO (User)
apiUserTransform CreateUserRequest {..} = do
    currTime <- getCurrentTime
    return $ 
        User { name = reqName,
               login = reqLogin,
               password = reqPassword,
               createDate = currTime,
               isAdmin = reqIsAdmin,
               isAbleToCreateNews = reqIsAbleToCreateNews
             }
