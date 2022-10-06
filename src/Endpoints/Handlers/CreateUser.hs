module Endpoints.Handlers.CreateUser where

import Control.Monad.Reader (ReaderT)
import Data.Maybe (isJust)
import qualified Data.Text as T
import qualified Types.API.User as API (CreateUserRequest (..))
import qualified Types.DB.User as DB (User (..))
import Types.Domain.Environment (Environment (..))

data Handle m = Handle
  { hFindUserByLogin :: T.Text -> ReaderT Environment m (Maybe Int),
    hWriteUser :: API.CreateUserRequest -> ReaderT Environment m ()
  }

data CreateUserResult = NotAdmin | LoginIsTaken | CreateUserSuccess deriving (Show, Eq)

hCreateUser :: Monad m => Handle m -> DB.User -> API.CreateUserRequest -> ReaderT Environment m CreateUserResult
hCreateUser Handle {..} invoker req =
  if not (DB.isAdmin invoker)
    then pure NotAdmin
    else do
      mbUserId <- hFindUserByLogin $ API.reqLogin req
      if isJust mbUserId
        then pure LoginIsTaken
        else do
          hWriteUser req
          pure CreateUserSuccess
