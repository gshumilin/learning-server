module Endpoints.CreateUser where

import Control.Monad.Reader (ReaderT)
import DatabaseQueries.User (findUserIdByLogin, writeUser)
import Endpoints.Handlers.CreateUser (CreateUserResult (..), Handle (..), hCreateUser)
import Log (addLog)
import Network.HTTP.Types (hContentType, status200, status400, status404)
import Network.Wai (Response, responseLBS)
import qualified Types.API.User as API (CreateUserRequest (..))
import qualified Types.DB.User as DB (User (..))
import Types.Domain.Environment (Environment (..))
import Types.Domain.Log (LogLvl (..))

createUser :: DB.User -> API.CreateUserRequest -> ReaderT Environment IO Response
createUser invoker req = do
  res <- hCreateUser handle invoker req
  case res of
    NotAdmin -> do
      addLog DEBUG "createUser-error: NotAdmin"
      pure $ responseLBS status404 [(hContentType, "text/plain")] "Forbidden"
    LoginIsTaken -> do
      addLog DEBUG "createUser-error: LoginIsTaken"
      pure $ responseLBS status400 [(hContentType, "text/plain")] "Bad Request: Incorrect login"
    CreateUserSuccess -> do
      addLog DEBUG "createUser: CreateUserSuccess"
      pure $ responseLBS status200 [(hContentType, "text/plain")] "all done"
  where
    handle =
      Handle
        { hFindUserByLogin = findUserIdByLogin,
          hWriteUser = writeUser
        }
