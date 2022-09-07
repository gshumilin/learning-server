module Endpoints.User where

import Control.Monad.Reader (ReaderT, asks, lift)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Time (getCurrentTime)
import DatabaseQueries.User (readUsers)
import Network.HTTP.Types (hContentType, status200)
import Network.Wai (Response, responseLBS)
import qualified Types.API.User as API (CreateUserRequest (..))
import Types.Domain.Environment (Environment (..))
import qualified Types.Domain.User as Domain (User (..), UsersList (..))

getUsers :: ReaderT Environment IO Response
getUsers = do
  conn <- asks dbConnection
  usersList <- lift $ readUsers conn
  let jsonUsersList = encodePretty (Domain.UsersList usersList)
  pure $ responseLBS status200 [(hContentType, "text/plain")] jsonUsersList

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
