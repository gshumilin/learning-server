module Endpoints.News where

import Auth
import Control.Monad.Reader
import Data.Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import DatabaseQueries.News (readNews, writeNews)
import Log (addLog)
import Network.HTTP.Types (hContentType, status200, status400, status404)
import Network.Wai
import qualified Types.API.News as API
import qualified Types.Database.User as DBType
import Types.Domain.Environment
import Types.Domain.Log
import qualified Types.Domain.News as Domain

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
          lift $ writeNews conn (DBType.userID invoker) newNews
          pure $ responseLBS status200 [(hContentType, "text/plain")] "all done"
