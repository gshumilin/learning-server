module Endpoints.News where

import Types.Domain.User
import Types.Domain.Environment
import Types.API.User
import qualified Types.Domain.News as Domain
import Types.Domain.Picture
import qualified Types.Database.News as DBType
import Endpoints.Categories (dbCategoryTransform)
import Database.PostgreSQL.Simple (Connection)
import DataBaseQueries.News (writeNews, parseNewsList)
import Network.HTTP.Types (hContentType, status200, status400)
import Network.Wai
import Data.Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import Control.Monad (mapM)
import Control.Monad.Reader
import Data.Time

dbNewsTransform :: DBType.News -> ReaderT Environment IO Domain.News
dbNewsTransform DBType.News {..} = do 
    transformedCat <- dbCategoryTransform categoryID
    currTime <- lift $ getCurrentTime
    return $ Domain.News {
            title = title,
            createDate = createDate,
            creator = (User "nam" "log" "pas" currTime True True),
            category = transformedCat,
            textContent = textContent,
            picturesArray = PicturesArray [],
            isPublished = isPublished }

getNewsList :: ReaderT Environment IO (Response)
getNewsList = do
    conn <- asks dbConnection
    bdNewsList <- lift $ parseNewsList conn
    newsList <- mapM dbNewsTransform bdNewsList
    let jsonNewsList = encodePretty newsList
    return $ responseLBS status200 [(hContentType, "text/plain")] $ jsonNewsList

createNews :: Request -> ReaderT Environment IO (Response)
createNews req = do
    conn <- asks dbConnection
    rawJSON <- lift $ getRequestBodyChunk req
    let req = decodeStrict rawJSON :: Maybe Domain.News
    case req of 
        Nothing -> do
            lift $ putStrLn "Invalid JSON"
            return $ responseLBS status400 [(hContentType, "text/plain")] $ "Bad Request: Invalid JSON\n"
        Just newNews -> do
            lift . putStrLn . show $ rawJSON
            lift $ writeNews conn newNews
            return $ responseLBS status200 [(hContentType, "text/plain")] $ "all done"

editNews = undefined