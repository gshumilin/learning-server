module Endpoints.EditNews where

import Endpoints.Handlers.EditNews (EditNewsResult(..), Handle (..), hEditNews)
import DatabaseQueries.News (readSpecificNews, rewriteNews)
import Types.Domain.Log (LogLvl(..))
import Log (addLog)
import Auth (authorization)
import Types.Domain.Environment (Environment(..))
import Database.PostgreSQL.Simple (Connection)
import qualified Types.API.News as API 
import qualified Types.Database.User as Database
import Network.HTTP.Types (status200, status400, status403, status404, hContentType)
import Network.Wai (Request, Response, responseLBS, getRequestBodyChunk)
import Data.Aeson (decodeStrict, FromJSON)
import Control.Monad.Reader (asks, ReaderT, lift)

editNews :: Database.User -> API.EditNewsRequest -> ReaderT Environment IO Response
editNews invoker editNewsRequest = do
    conn <- asks dbConnection
    res <- lift $ hEditNews (handle conn) invoker editNewsRequest
    case res of
        NotAuthor -> return $ responseLBS status403 [(hContentType, "text/plain")] $ "Forbidden"
        NewsNotExists -> return $ responseLBS status404 [(hContentType, "text/plain")] $ "Forbidden"
        EditNewsSuccess -> return $ responseLBS status200 [(hContentType, "text/plain")] $ "all done"
    where 
        handle :: Connection -> Handle IO
        handle conn = Handle { hReadSpecificNews = \x -> readSpecificNews conn x,
                               hRewriteNews = \editedNewsFields editNewsRequest -> rewriteNews conn editedNewsFields editNewsRequest
                             }