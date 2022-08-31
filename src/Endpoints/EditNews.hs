module Endpoints.EditNews where

import Auth (authorization)
import Control.Monad.Reader (ReaderT, asks, lift)
import Data.Aeson (FromJSON, decodeStrict)
import Database.PostgreSQL.Simple (Connection)
import DatabaseQueries.News (readSpecificNews, rewriteNews)
import Endpoints.Handlers.EditNews (EditNewsResult (..), Handle (..), hEditNews)
import Log (addLog)
import Network.HTTP.Types (hContentType, status200, status400, status403, status404)
import Network.Wai (Request, Response, getRequestBodyChunk, responseLBS)
import qualified Types.API.News as API
import qualified Types.Database.User as Database
import Types.Domain.Environment (Environment (..))
import Types.Domain.Log (LogLvl (..))

editNews :: Database.User -> API.EditNewsRequest -> ReaderT Environment IO Response
editNews invoker editNewsRequest = do
  conn <- asks dbConnection
  res <- lift $ hEditNews (handle conn) invoker editNewsRequest
  case res of
    NotAuthor -> pure $ responseLBS status403 [(hContentType, "text/plain")] "Forbidden"
    NewsNotExists -> pure $ responseLBS status404 [(hContentType, "text/plain")] "Forbidden"
    EditNewsSuccess -> pure $ responseLBS status200 [(hContentType, "text/plain")] "all done"
  where
    handle :: Connection -> Handle IO
    handle conn =
      Handle
        { hReadSpecificNews = readSpecificNews conn,
          hRewriteNews = rewriteNews conn
        }
