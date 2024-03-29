module Endpoints.EditCategory where

import Control.Monad.Reader (ReaderT)
import DatabaseQueries.Category (readCategoryById, readCategoryByTitle, rewriteCategory)
import Endpoints.Handlers.EditCategory (EditCategoryResult (..), Handle (..), hEditCategory)
import Log (addLog)
import Network.HTTP.Types (hContentType, status200, status400, status404)
import Network.Wai (Response, responseLBS)
import qualified Types.API.Category as API (EditCategoryRequest (..))
import qualified Types.DB.User as DB (User (..))
import Types.Domain.Environment (Environment (..))
import Types.Domain.Log (LogLvl (..))

editCategory :: DB.User -> API.EditCategoryRequest -> ReaderT Environment IO Response
editCategory invoker editCategoryRequest = do
  res <- hEditCategory handle invoker editCategoryRequest
  case res of
    NotAdmin -> do
      addLog DEBUG "editCategory-error: NotAdmin"
      pure $ responseLBS status404 [(hContentType, "text/plain")] "Forbidden"
    CategoryNotExists -> do
      addLog DEBUG "editCategory-error: CategoryNotExists"
      pure $ responseLBS status400 [(hContentType, "text/plain")] "Bad Request: There is no category with such Id"
    IncorrectParentId -> do
      addLog DEBUG "editCategory-error: IncorrectParentId"
      pure $ responseLBS status400 [(hContentType, "text/plain")] "Bad Request: Incorrect Parent Id"
    IncorrectTitle -> do
      addLog DEBUG "editCategory-error: IncorrectTitle"
      pure $ responseLBS status400 [(hContentType, "text/plain")] "Bad Request: Incorrect title"
    EditCategorySuccess -> do
      addLog DEBUG "editCategory: Success"
      pure $ responseLBS status200 [(hContentType, "text/plain")] "all done"
  where
    handle :: Handle (ReaderT Environment IO)
    handle =
      Handle
        { hReadCategoryById = readCategoryById,
          hReadCategoryByTitle = readCategoryByTitle,
          hRewriteCategory = rewriteCategory
        }
