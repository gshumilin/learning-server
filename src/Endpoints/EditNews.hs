module Endpoints.EditNews where

import Control.Monad.Reader (ReaderT, asks, lift)
import Database.PostgreSQL.Simple (Connection)
import DatabaseQueries.News (readSpecificNews, rewriteNews)
import Endpoints.Handlers.EditNews (EditNewsResult (..), Handle (..), hEditNews)
import Network.HTTP.Types (hContentType, status200, status404)
import Network.Wai (Response, responseLBS)
import qualified Types.API.News as API (EditNewsRequest (..))
import qualified Types.DB.User as DB (User (..))
import Types.Domain.Environment (Environment (..))

editNews :: DB.User -> API.EditNewsRequest -> ReaderT Environment IO Response
editNews invoker editNewsRequest = do
  conn <- asks dbConnection
  res <- lift $ hEditNews (handle conn) editNewsRequest
  case res of
    NewsNotExistsForThisAuthor -> pure $ responseLBS status404 [(hContentType, "text/plain")] "Forbidden"
    EditNewsSuccess -> pure $ responseLBS status200 [(hContentType, "text/plain")] "all done"
  where
    handle :: Connection -> Handle IO
    handle conn =
      Handle
        { hReadSpecificNews = readSpecificNews conn (DB.userId invoker),
          hRewriteNews = rewriteNews conn
        }
