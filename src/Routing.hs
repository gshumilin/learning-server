module Routing where

import Types.Domain.User
import Types.Domain.Environment
import qualified Endpoints.User
import qualified Endpoints.News
import Endpoints.Categories
import Network.Wai
import qualified Data.ByteString.Char8 as BS
import Control.Monad.Reader
import DataBaseQueries.GetConnection (getConnection)

application :: Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
application req respond = do
    env <- parseEnvironment
    runReaderT (routing req respond) env

routing :: Request -> (Response -> IO ResponseReceived) -> ReaderT Environment IO ResponseReceived
routing req respond = do
    let reqPath = rawPathInfo req
    let reqQuery = rawQueryString req
    case reqPath of
        "/getUsersList" -> do
            res <- Endpoints.User.getUsersList
            lift $ respond res
        "/createUser"   -> do
            res <- Endpoints.User.createUser req
            lift $ respond res
        "/getNewsList"  -> do
            res <- Endpoints.News.getNewsList
            lift $ respond res
        "/createNews"   -> do
            res <- Endpoints.News.createNews req
            lift $ respond res    
        "/editNews"     -> do
            res <- Endpoints.News.editNews
            lift $ respond res
        "/getCategoriesList" -> do
            res <- Endpoints.Categories.getCategoriesList    
            lift $ respond res
        "/createCategory" -> do
            res <- Endpoints.Categories.createCategory req
            lift $ respond res
        _               -> error "Unknown method"

parseEnvironment :: IO (Environment)
parseEnvironment = do
    conn <- getConnection
    return (Environment conn)
