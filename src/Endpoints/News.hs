module Endpoints.News where

import Types.Domain.User
import Types.Domain.Environment
import Types.API.User
import qualified Types.Domain.News as Domain
import Types.Domain.Picture
import qualified Types.Database.News as Database
import Endpoints.Categories (dbCategoryTransform)
import Database.PostgreSQL.Simple (Connection)
import DataBaseQueries.News (writeNews, parseNewsList)
import DataBaseQueries.User (findUser)
import DataBaseQueries.Picture (findPicturesArray)
import Network.HTTP.Types (hContentType, status200, status400)
import Network.Wai
import Data.Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import Control.Monad (mapM)
import Control.Monad.Reader
import Data.Time

dbNewsTransform :: Database.News -> ReaderT Environment IO Domain.News
dbNewsTransform Database.News {..} = do 
    conn <- asks dbConnection
    newsCategory <- dbCategoryTransform categoryID             --refactoring!
    newsCreator <- lift $ findUser conn creatorID              --undefined
    newsPicturesArray <- lift $ findPicturesArray conn newsID  --undefined
    return $ Domain.News {
            title = title,
            createDate = createDate,
            creator = newsCreator,
            category = newsCategory,
            textContent = textContent,
            picturesArray = newsPicturesArray,
            isPublished = isPublished }

getNewsList :: ReaderT Environment IO (Response)
getNewsList = do
    conn <- asks dbConnection
    bdNewsList <- lift $ parseNewsList conn
    newsList <- mapM dbNewsTransform bdNewsList
    let jsonNewsList = encodePretty $ Domain.NewsList newsList
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