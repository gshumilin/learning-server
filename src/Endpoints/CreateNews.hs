module Endpoints.CreateNews where

import Control.Monad.Reader (ReaderT, asks, lift)
import Database.PostgreSQL.Simple (Connection)
import DatabaseQueries.Category (readCategoryById)
import DatabaseQueries.News (writeNews)
import Endpoints.Handlers.CreateNews (CreateNewsResult (..), Handle (..), hCreateNews)
import Log (addLog)
import Network.HTTP.Types (hContentType, status200, status400, status403)
import Network.Wai (Response, responseLBS)
import qualified Types.API.News as API (CreateNewsRequest (..))
import qualified Types.DB.User as DB (User (..))
import Types.Domain.Environment (Environment (..))
import Types.Domain.Log (LogLvl (..))

createNews :: DB.User -> API.CreateNewsRequest -> ReaderT Environment IO Response
createNews invoker req = do
  conn <- asks dbConnection
  res <- lift $ hCreateNews (handle conn) invoker req
  case res of
    NotAbleToCreateNews -> do
      addLog DEBUG "createNews-error: NotAbleToCreateNews"
      pure $ responseLBS status403 [(hContentType, "text/plain")] "Forbidden"
    CategoryNotExists -> do
      addLog DEBUG "createNews-error: CategoryNotExists"
      pure $ responseLBS status400 [(hContentType, "text/plain")] "Bad Request: There is no category with such ID"
    CreateNewsSuccess -> do
      addLog DEBUG "createNews: Success"
      pure $ responseLBS status200 [(hContentType, "text/plain")] "all done"
  where
    handle :: Connection -> Handle IO
    handle conn =
      Handle
        { hReadCategoryById = readCategoryById conn,
          hWriteNews = writeNews conn
        }
