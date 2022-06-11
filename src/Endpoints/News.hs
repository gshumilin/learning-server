module Endpoints.News where

import Types.User
import Types.API.User
import Types.Environment
import Types.News
import Database.PostgreSQL.Simple (Connection)
import DataBaseQueries (writeNews, parseNewsList)
import Network.HTTP.Types (hContentType, status200, status400)
import Network.Wai
import Data.Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import Control.Monad.Reader

getNewsList :: ReaderT Environment IO (Response)
getNewsList = do
    conn <- asks dbConnection
    newsList <- lift $ DataBaseQueries.parseNewsList conn
    let jsonNewsList = encodePretty newsList
    return $ responseLBS status200 [(hContentType, "text/plain")] $ jsonNewsList

createNews :: Request -> ReaderT Environment IO (Response)
createNews req = do
    conn <- asks dbConnection
    rawJSON <- lift $ getRequestBodyChunk req
    let req = decodeStrict rawJSON :: Maybe News
    case req of 
        Nothing -> do
            lift $ putStrLn "Invalid JSON"
            return $ responseLBS status400 [(hContentType, "text/plain")] $ "Bad Request: Invalid JSON\n"
        Just newNews -> do
            lift . putStrLn . show $ rawJSON
            lift $ DataBaseQueries.writeNews conn newNews
            return $ responseLBS status200 [(hContentType, "text/plain")] $ "all done"

editNews = undefined