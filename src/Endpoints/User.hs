module Endpoints.User where

import Control.Monad.Reader (ReaderT, asks, lift)
import Data.Aeson (decodeStrict)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Time (getCurrentTime)
import DatabaseQueries.User (readUsers, writeUser)
import Network.HTTP.Types (hContentType, status200, status400)
import Network.Wai (Request, Response, getRequestBodyChunk, responseLBS)
import qualified Types.API.User as API (CreateUserRequest (..))
import Types.Domain.Environment (Environment (..))
import qualified Types.Domain.User as Domain (User (..), UsersList (..))

getUsers :: ReaderT Environment IO Response
getUsers = do
  conn <- asks dbConnection
  usersList <- lift $ readUsers conn
  let jsonUsersList = encodePretty (Domain.UsersList usersList)
  pure $ responseLBS status200 [(hContentType, "text/plain")] jsonUsersList

createUser :: Request -> ReaderT Environment IO Response
createUser r = do
  conn <- asks dbConnection
  rawJSON <- lift $ getRequestBodyChunk r
  let req = decodeStrict rawJSON :: Maybe API.CreateUserRequest
  case req of
    Nothing -> do
      lift $ putStrLn "Invalid JSON"
      lift $ print rawJSON
      pure $ responseLBS status400 [(hContentType, "text/plain")] "Bad Request: Invalid JSON\n"
    Just createUserReq -> do
      newUser <- lift $ apiUserTransform createUserReq
      lift $ writeUser conn newUser
      pure $ responseLBS status200 [(hContentType, "text/plain")] "all done"

apiUserTransform :: API.CreateUserRequest -> IO Domain.User
apiUserTransform API.CreateUserRequest {..} = do
  currTime <- getCurrentTime
  pure $
    Domain.User
      { name = reqName,
        login = reqLogin,
        password = reqPassword,
        createDate = currTime,
        isAdmin = reqIsAdmin,
        isAbleToCreateNews = reqIsAbleToCreateNews
      }
