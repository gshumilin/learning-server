module Endpoints.News where

import Types.Domain.User
import Types.Domain.Environment
import Types.API.User
import qualified Types.Domain.News as Domain
import qualified Types.API.News as API
import Types.Domain.Picture
import qualified Types.Database.News as Database
import Endpoints.Categories (dbCategoryTransform, getSpecificCategory)
import Database.PostgreSQL.Simple (Connection)
import DataBaseQueries.News (writeNews, rewriteNews, parseNewsList)
import DataBaseQueries.User (findUser)
import DataBaseQueries.Picture (findPicturesArray)
import Network.HTTP.Types (hContentType, status200, status400)
import Network.Wai
import Data.Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import Control.Monad (mapM)
import Control.Monad.Reader
import Data.Time
import Data.List (find)

dbNewsTransform :: Database.News -> ReaderT Environment IO Domain.News
dbNewsTransform Database.News {..} = do 
    conn <- asks dbConnection
    newsCategory <- getSpecificCategory categoryID             --refactoring!
    newsCreator <- lift $ findUser conn creatorID              --refactoring! unsafe 'head' is used
    newsPicturesArray <- lift $ findPicturesArray conn newsID  --undefined
    return $ Domain.News {
            title = title,
            createDate = createDate,
            creator = newsCreator,
            category = newsCategory,
            textContent = textContent,
            picturesArray = newsPicturesArray,
            isPublished = isPublished }

apiNewsTransform :: API.CreateNewsRequest -> ReaderT Environment IO Domain.News
apiNewsTransform API.CreateNewsRequest {..} = do
    conn <- asks dbConnection
    currTime <- lift $ getCurrentTime
    newsCreator <- lift $ findUser conn 1             --HARD_CODE Sample userID
    category' <- dbCategoryTransform categoryID
    return $ 
        Domain.News { title = "Test",
                      createDate = currTime,
                      creator = newsCreator,
                      category = category',
                      textContent = textContent,
                      picturesArray = picturesArray,
                      isPublished = False
                    }

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
    let req = decodeStrict rawJSON :: Maybe API.CreateNewsRequest
    case req of 
        Nothing -> do
            lift $ putStrLn "Invalid JSON"
            return $ responseLBS status400 [(hContentType, "text/plain")] $ "Bad Request: Invalid JSON\n"
        Just newNews -> do
            lift . putStrLn . show $ rawJSON
            transformedNews <- apiNewsTransform newNews
            lift $ writeNews conn $ transformedNews
            return $ responseLBS status200 [(hContentType, "text/plain")] $ "all done"

editNews :: Request -> ReaderT Environment IO (Response)
editNews request = do
    conn <- asks dbConnection
    rawJSON <- lift $ getRequestBodyChunk request
    let decodedReq = decodeStrict rawJSON :: Maybe API.EditNewsRequest
    case decodedReq of 
        Nothing -> do
            lift $ putStrLn "Invalid JSON"
            return $ responseLBS status400 [(hContentType, "text/plain")] $ "Bad Request: Invalid JSON\n"
        Just editedNews -> do
            lift . putStrLn . show $ rawJSON
            lift $ rewriteNews conn editedNews
            return $ responseLBS status200 [(hContentType, "text/plain")] $ "all done"