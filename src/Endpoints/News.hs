module Endpoints.News where

import Types.Domain.Log
import Log (addLog)
import Auth
import Types.Domain.User
import Types.Domain.Environment
import Types.API.User
import qualified Types.Domain.News as Domain
import qualified Types.API.News as API
import Types.Domain.Picture
import qualified Types.Database.News as DBType
import qualified Types.Database.User as DBType
import Endpoints.Categories (dbCategoryTransform, getSpecificCategory)
import Database.PostgreSQL.Simple (Connection)
import DatabaseQueries.News (writeNews, rewriteNews, readNews, readSpecificNews)
import DatabaseQueries.Picture (parsePicturesLinks)
import Network.HTTP.Types (hContentType, status200, status400, status404)
import Network.Wai
import Network.HTTP.Types.URI
import Data.Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Char8 as BS
import Control.Monad (mapM)
import Control.Monad.Reader
import Data.List (find)
import qualified Database.PostgreSQL.Simple.Types as Postgres

getNews :: Request -> ReaderT Environment IO (Response)
getNews req = do
    newsList <- readNews req
    let jsonNewsList = encodePretty $ Domain.NewsList newsList
    return $ responseLBS status200 [(hContentType, "text/plain")] $ jsonNewsList

editNews :: Request -> ReaderT Environment IO (Response)
editNews request = do
    conn <- asks dbConnection
    rawJSON <- lift $ getRequestBodyChunk request
    eiClientsUser <- lift $ authorization conn request
    case eiClientsUser of
        Left err -> return $ responseLBS status404 [(hContentType, "text/plain")] $ "404 : Not Found"
        Right clientsUser -> do
            let decodedReq = decodeStrict rawJSON :: Maybe API.EditNewsRequest
            case decodedReq of 
                Nothing -> do
                    lift $ putStrLn "Invalid JSON" -- log
                    return $ responseLBS status400 [(hContentType, "text/plain")] $ "Bad Request: Invalid JSON\n"
                Just editedNews -> do
                    currNews <- lift $ readSpecificNews conn (API.newsID editedNews) (DBType.userID clientsUser)
                    lift $ rewriteNews conn currNews editedNews
                    return $ responseLBS status200 [(hContentType, "text/plain")] $ "all done"

createNews :: Request -> ReaderT Environment IO (Response)
createNews req = do
    conn <- asks dbConnection
    authUser <- lift $ authorization conn req
    case authUser of 
        Left err -> return $ responseLBS status404 [(hContentType, "text/plain")] $ "404 Not Found\n"
        Right clientsUser -> do
            rawJSON <- lift $ getRequestBodyChunk req
            let apiReq = decodeStrict rawJSON :: Maybe API.CreateNewsRequest
            case apiReq of 
                Nothing -> do
                    addLog DEBUG $ "----- createNews returned \"Invalid JSON\""
                    return $ responseLBS status400 [(hContentType, "text/plain")] $ "Bad Request: Invalid JSON\n"
                Just newNews -> do
                    lift $ writeNews conn (DBType.userID clientsUser) newNews
                    return $ responseLBS status200 [(hContentType, "text/plain")] $ "all done"