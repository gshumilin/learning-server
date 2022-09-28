module Endpoints.Handlers.CreateUser where

import Data.Maybe (isJust)
import qualified Data.Text as T
import qualified Types.API.User as API (CreateUserRequest (..))
import qualified Types.DB.User as DB (User (..))

data Handle m = Handle
  { hFindUserByLogin :: T.Text -> m (Maybe Int),
    hWriteUser :: API.CreateUserRequest -> m Int
  }

data CreateUserResult = NotAdmin | LoginIsTaken | CreateUserSuccess Int deriving (Show, Eq)

hCreateUser :: Monad m => Handle m -> DB.User -> API.CreateUserRequest -> m CreateUserResult
hCreateUser Handle {..} invoker req =
  if not (DB.isAdmin invoker)
    then pure NotAdmin
    else do
      mbUserId <- hFindUserByLogin $ API.reqLogin req
      if isJust mbUserId
        then pure LoginIsTaken
        else do
          resId <- hWriteUser req
          pure $ CreateUserSuccess resId
