module MainHandle where

import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Types.API.User as API (CreateUserRequest (..))
import qualified Types.DB.User as DB (User (..))

data RoutingResult = UnknownMethod | UnknownEndpoint | RoutingSuccess deriving (Show, Eq)

data RoutingHandle m req resp = MainHandle
  { hGetMethod :: req -> Text,
    hGetEndpoint :: req -> Text,
    hUnknownMethodResp :: resp,
    hUnknownEndpointResp :: resp,
    hAuth :: req -> m DB.User,
    createUserHandler :: CreateUserHandle m req resp
  }

hRouting :: Monad m => RoutingHandle m req resp -> req -> m (RoutingResult, resp)
hRouting MainHandle {..} request = do
  case hGetMethod request of
    "POST" ->
      case hGetEndpoint request of
        "/createUser" -> do
          invoker <- hAuth request
          (_, response) <- hCreateUser createUserHandler invoker request
          pure (RoutingSuccess, response)
        "/createCategory" -> undefined
        "/createNews" -> undefined
        _ -> pure (UnknownEndpoint, hUnknownEndpointResp)
    _ -> pure (UnknownMethod, hUnknownMethodResp)

-- ------------- ------------- ------------- ------------- ------------- ------------- -------------

data CreateUserResult = IncorrectRequest | NotAdmin | LoginIsTaken | CreateUserSuccess deriving (Show, Eq)

data CreateUserHandle m req resp = CreateUserHandle
  { hParseRequest :: req -> Maybe API.CreateUserRequest,
    hFindUserByLogin :: Text -> m (Maybe Int),
    hWriteUser :: API.CreateUserRequest -> m (),
    hIncorrectRequestResp :: resp,
    hNotAdminResp :: resp,
    hLoginIsTakenResp :: resp,
    hCreateUserSuccessResp :: resp
  }

hCreateUser :: Monad m => CreateUserHandle m req resp -> DB.User -> req -> m (CreateUserResult, resp)
hCreateUser CreateUserHandle {..} DB.User {..} r = do
  case hParseRequest r of
    Nothing -> pure (IncorrectRequest, hIncorrectRequestResp)
    Just req@API.CreateUserRequest {..} ->
      if not isAdmin
        then pure (NotAdmin, hNotAdminResp)
        else do
          mbUserId <- hFindUserByLogin reqLogin
          if isJust mbUserId
            then pure (LoginIsTaken, hLoginIsTakenResp)
            else do
              hWriteUser req
              pure (CreateUserSuccess, hCreateUserSuccessResp)
