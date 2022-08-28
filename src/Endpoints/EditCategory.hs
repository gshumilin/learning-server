module Endpoints.EditCategory where

import Endpoints.Handlers.EditCategory (EditCategoryResult(..), Handle (..), hEditCategory)
import DatabaseQueries.Category (readCategoryById, readCategoryByTitle, rewriteCategory)
import Types.Domain.Log (LogLvl(..))
import Log (addLog)
import Types.Domain.Environment (Environment(..))
import Database.PostgreSQL.Simple (Connection)
import qualified Types.API.Category as API 
import qualified Types.Database.User as DB
import Network.HTTP.Types (status200, status400, status403, hContentType)
import Network.Wai (Response, responseLBS)
import Data.Aeson (decodeStrict, FromJSON)
import Control.Monad.Reader (asks, ReaderT, lift)

editCategory :: DB.User -> API.EditCategoryRequest -> ReaderT Environment IO Response
editCategory invoker editCategoryRequest = do
    conn <- asks dbConnection
    res <- lift $ hEditCategory (handle conn) invoker editCategoryRequest
    case res of
        NotAdmin -> do
            addLog DEBUG "editCategory-error: NotAdmin"
            return $ responseLBS status403 [(hContentType, "text/plain")] $ "Forbidden"
        CategoryNotExists -> do
            addLog DEBUG "editCategory-error: CategoryNotExists"
            return $ responseLBS status400 [(hContentType, "text/plain")] $ "Bad Request: There is no category with such ID"
        IncorrectParentId -> do
            addLog DEBUG "editCategory-error: IncorrectParentId"
            return $ responseLBS status400 [(hContentType, "text/plain")] $ "Bad Request: Incorrect Parent ID"
        IncorrectTitle -> do
            addLog DEBUG "editCategory-error: IncorrectTitle"
            return $ responseLBS status400 [(hContentType, "text/plain")] $ "Bad Request: Incorrect title"
        EditCategorySuccess -> do
            addLog DEBUG "editCategory: Success"
            return $ responseLBS status200 [(hContentType, "text/plain")] $ "all done"
    where 
        handle :: Connection -> Handle IO
        handle conn = Handle {  hReadCategoryById = \catId -> readCategoryById conn catId,
                                hReadCategoryByTitle = \catTitle -> readCategoryByTitle conn catTitle,
                                hRewriteCategory = \req -> rewriteCategory conn req
                             }