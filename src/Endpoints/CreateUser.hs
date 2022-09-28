module Endpoints.CreateUser where

import Control.Monad.Reader (ReaderT, asks, lift)
import DatabaseQueries.User (findUserIdByLogin, writeUser)
import Endpoints.Handlers.CreateUser (CreateUserResult (..), Handle (..), hCreateUser)
import Log (addLog)
import Network.HTTP.Types (hContentType, status200, status400, status403)
import Network.Wai (Response, responseLBS)
import qualified Types.API.User as API (CreateUserRequest (..))
import qualified Types.DB.User as DB (User (..))
import Types.Domain.Environment (Environment (..))
import Types.Domain.Log (LogLvl (..))

createUser :: DB.User -> API.CreateUserRequest -> ReaderT Environment IO Response
createUser invoker req = do
  conn <- asks dbConnection
  res <- lift $ hCreateUser (handle conn) invoker req
  case res of
    NotAdmin -> do
      addLog DEBUG "createUser-error: NotAdmin"
      pure $ responseLBS status403 [(hContentType, "text/plain")] "Forbidden"
    LoginIsTaken -> do
      addLog DEBUG "createUser-error: LoginIsTaken"
      pure $ responseLBS status400 [(hContentType, "text/plain")] "Bad Request: Incorrect title"
    CreateUserSuccess -> do
      addLog DEBUG "createUser: CreateUserSuccess"
      pure $ responseLBS status200 [(hContentType, "text/plain")] "all done"
  where
    handle conn =
      Handle
        { hFindUserByLogin = findUserIdByLogin conn,
          hWriteUser = writeUser conn
        }
