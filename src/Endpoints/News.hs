module Endpoints.News where

import Auth (authorization)
import Control.Monad.Reader (ReaderT, asks, lift)
import Data.Aeson (decodeStrict)
import Data.Aeson.Encode.Pretty (encodePretty)
import DatabaseQueries.News (readNews, writeNews)
import Log (addLog)
import Network.HTTP.Types (hContentType, status200, status400, status404)
import Network.Wai (Request, Response, getRequestBodyChunk, responseLBS)
import qualified Types.API.News as API (CreateNewsRequest (..))
import qualified Types.DB.User as DB (User (..))
import Types.Domain.Environment (Environment (..))
import Types.Domain.Log (LogLvl (..))
import qualified Types.Domain.News as Domain (NewsList (..))

getNews :: Request -> ReaderT Environment IO Response
getNews req = do
  newsList <- readNews req
  let jsonNewsList = encodePretty $ Domain.NewsList newsList
  pure $ responseLBS status200 [(hContentType, "text/plain")] jsonNewsList

createNews :: Request -> ReaderT Environment IO Response
createNews req = do
  conn <- asks dbConnection
  authUser <- lift $ authorization conn req
  case authUser of
    Left _ -> pure $ responseLBS status404 [(hContentType, "text/plain")] "404 Not Found\n"
    Right invoker -> do
      rawJSON <- lift $ getRequestBodyChunk req
      let apiReq = decodeStrict rawJSON :: Maybe API.CreateNewsRequest
      case apiReq of
        Nothing -> do
          addLog DEBUG "----- createNews pureed \"Invalid JSON\""
          pure $ responseLBS status400 [(hContentType, "text/plain")] "Bad Request: Invalid JSON\n"
        Just newNews -> do
          lift $ writeNews conn (DB.userID invoker) newNews
          pure $ responseLBS status200 [(hContentType, "text/plain")] "all done"
