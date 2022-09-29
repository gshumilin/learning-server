module Endpoints.CreateCategory where

import Control.Monad.Reader (ReaderT, lift)
import Data.ByteString.Char8 (pack)
import Data.ByteString.Lazy (fromStrict)
import Database.PostgreSQL.Simple (Connection)
import DatabaseQueries.Category (readCategoryById, readCategoryByTitle, writeCategory)
import Endpoints.Handlers.CreateCategory (CreateCategoryResult (..), Handle (..), hCreateCategory)
import Log (addLog)
import Network.HTTP.Types (hContentType, status200, status400, status403)
import Network.Wai (Response, responseLBS)
import qualified Types.API.Category as API (CreateCategoryRequest (..))
import qualified Types.DB.User as DB (User (..))
import Types.Domain.Environment (Environment (..))
import Types.Domain.Log (LogLvl (..))
import Utils (askConnection)

createCategory :: DB.User -> API.CreateCategoryRequest -> ReaderT Environment IO Response
createCategory invoker createCategoryRequest = do
  conn <- askConnection
  res <- lift $ hCreateCategory (handle conn) invoker createCategoryRequest
  case res of
    NotAdmin -> do
      addLog DEBUG "createCategory-error: NotAdmin"
      pure $ responseLBS status403 [(hContentType, "text/plain")] "Forbidden"
    IncorrectParentId -> do
      addLog DEBUG "createCategory-error: IncorrectParentId"
      pure $ responseLBS status400 [(hContentType, "text/plain")] "Bad Request: Incorrect Parent Id"
    IncorrectTitle -> do
      addLog DEBUG "createCategory-error: IncorrectTitle"
      pure $ responseLBS status400 [(hContentType, "text/plain")] "Bad Request: Incorrect title"
    CreateCategorySuccess resId -> do
      addLog DEBUG "createCategory: Success"
      let reqRes = fromStrict . pack . show $ resId
      pure $ responseLBS status200 [(hContentType, "text/plain")] reqRes
  where
    handle :: Connection -> Handle IO
    handle conn =
      Handle
        { hReadCategoryById = readCategoryById conn,
          hReadCategoryByTitle = readCategoryByTitle conn,
          hWriteCategory = writeCategory conn
        }
