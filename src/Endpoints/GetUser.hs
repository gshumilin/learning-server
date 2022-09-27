module Endpoints.GetUser where

import Control.Monad.Reader (ReaderT, asks, lift)
import Data.Aeson.Encode.Pretty (encodePretty)
import DatabaseQueries.User (readUsers)
import Network.HTTP.Types (hContentType, status200)
import Network.Wai (Response, responseLBS)
import Types.Domain.Environment (Environment (..))
import qualified Types.Domain.User as Domain (UsersList (..))

getUsers :: ReaderT Environment IO Response
getUsers = do
  conn <- asks dbConnection
  usersList <- lift $ readUsers conn
  let jsonUsersList = encodePretty (Domain.UsersList usersList)
  pure $ responseLBS status200 [(hContentType, "text/plain")] jsonUsersList
