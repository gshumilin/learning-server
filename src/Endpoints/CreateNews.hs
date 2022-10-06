module Endpoints.CreateNews where

import Control.Monad.Reader (ReaderT)
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
import Utils (intToLBS)

createNews :: DB.User -> API.CreateNewsRequest -> ReaderT Environment IO Response
createNews invoker req = do
  res <- hCreateNews handle invoker req
  case res of
    NotAbleToCreateNews -> do
      addLog DEBUG "createNews-error: NotAbleToCreateNews"
      pure $ responseLBS status403 [(hContentType, "text/plain")] "Forbidden"
    InvalidPictureFormat -> do
      addLog DEBUG "createNews-error: InvalidPictureFormat"
      pure $ responseLBS status400 [(hContentType, "text/plain")] "Bad Request: invalid picture format"
    CategoryNotExists -> do
      addLog DEBUG "createNews-error: CategoryNotExists"
      pure $ responseLBS status400 [(hContentType, "text/plain")] "Bad Request: there is no category with such Id"
    CreateNewsSuccess resId -> do
      let reqRes = intToLBS resId
      addLog DEBUG "createNews: Success"
      pure $ responseLBS status200 [(hContentType, "text/plain")] reqRes
  where
    handle :: Handle IO
    handle =
      Handle
        { hReadCategoryById = readCategoryById,
          hWriteNews = writeNews
        }
