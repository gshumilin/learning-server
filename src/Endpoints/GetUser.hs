module Endpoints.GetUser where

import Control.Monad.Reader (ReaderT)
import Data.Aeson.Encode.Pretty (encodePretty)
import DatabaseQueries.User (readUsers)
import Network.HTTP.Types (hContentType, status200)
import Network.Wai (Response, responseLBS)
import Types.Domain.Environment (Environment (..))

getUsers :: ReaderT Environment IO Response
getUsers = do
  usersList <- readUsers
  let jsonUsersList = encodePretty usersList
  pure $ responseLBS status200 [(hContentType, "text/plain")] jsonUsersList
