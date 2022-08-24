module DatabaseQueries.News where

import Types.Domain.Log
import Log (addLog)
import qualified Types.Domain.News as Domain
import qualified Types.Domain.Picture as Domain
import qualified Types.Domain.User as Domain
import qualified Types.Database.News as DBType
import Types.Domain.Environment
import qualified Types.API.News as API
import DatabaseQueries.QueryCreator (makeReadNewsQuery)
import DatabaseQueries.Picture (parsePicturesLinks, deleteNewsPictures, addPicturesToNews)
import DatabaseQueries.User (findUser)
import Endpoints.Categories (getSpecificCategory)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Types
import qualified Data.Text as T
import Data.Time
import Data.Maybe (fromMaybe)
import Control.Monad.Reader
import qualified Data.ByteString.Char8 as BS
import Network.Wai

readNews :: Request -> ReaderT Environment IO [Domain.News]
readNews req = do
    addLog DEBUG "----- Started readNews \n"
    conn <- asks dbConnection
    mbQuery <- lift $ makeReadNewsQuery conn req
    case mbQuery of 
        Nothing -> return []
        Just q -> do
            let (Query bsQ) = q
            addLog DEBUG $ show $ "----- made this psql-request: \n\"" <> bsQ <> "\"\n"
            dbNews <- lift $ query_ conn q :: ReaderT Environment IO [DBType.News]
            addLog DEBUG $ "----- got this psql News List: \"" ++ (show dbNews) ++ "\"\n"
            res <- lift $ mapM (fromDbNews conn) dbNews
            return res

fromDbNews :: Connection -> DBType.News -> IO Domain.News
fromDbNews conn DBType.News {..} = do 
    newsCategory <- getSpecificCategory conn categoryID
    newsCreator <- findUser conn creatorID
    newsPictures <- parsePicturesLinks conn newsID
    return $ Domain.News 
        {   newsID = newsID,
            title = title,
            createDate = createDate,
            creator = newsCreator,
            category = newsCategory,
            textContent = textContent,
            picturesLinks = newsPictures,
            isPublished = isPublished, 
            numbersOfPictures = numbersOfPictures
        }

readSpecificNews :: Connection -> Int -> Int -> IO DBType.EditedNewsFields
readSpecificNews conn newsID creatorID = do
    let q = "SELECT creator_id, title, category_id, text_content FROM news WHERE id=? AND creator_id=?"
    [res] <- query conn q (newsID, creatorID) :: IO [DBType.EditedNewsFields]
    return res 

writeNews :: Connection -> Int -> API.CreateNewsRequest -> IO ()
writeNews conn newsCreatorID API.CreateNewsRequest {..} = do
    currTime <- getCurrentTime
    let isPublished = False
    let q = "INSERT INTO news (title, create_date, creator_id, category_id, text_content, is_published) values (?,?,?,?,?,?) RETURNING id"
    [Only newsId] <- query conn q (title, currTime, newsCreatorID, categoryID, textContent, isPublished) :: IO [Only Int]
    case pictures of
        Nothing -> return ()
        Just picArr -> do
            mapM ( \Domain.Picture {..} -> do
                let q = "INSERT INTO pictures (data,mime) values (?,?) RETURNING id"
                [Only picID] <- query conn q (picData, mime) :: IO [Only Int]
                let q' = "INSERT INTO news_pictures (news_id, picture_id) values (?,?)"
                execute conn q' (newsId, picID)            
                ) picArr
            return ()

rewriteNews :: Connection -> DBType.EditedNewsFields -> API.EditNewsRequest -> IO ()
rewriteNews conn editedNewsFields editNewsRequest = do 
    let q = "UPDATE news SET title=?, category_id=?, text_content=? WHERE id=?"
    execute conn q ( fromMaybe (DBType.oldTitle editedNewsFields) (API.newTitle editNewsRequest),
                     fromMaybe (DBType.oldCategoryID editedNewsFields) (API.newCategoryID editNewsRequest),
                     fromMaybe (DBType.oldTextContent editedNewsFields) (API.newTextContent editNewsRequest),
                     API.newsID editNewsRequest
                   )
    case API.newPictures editNewsRequest of
        Nothing -> return ()
        Just [] -> do 
            deleteNewsPictures conn (API.newsID editNewsRequest)
            return ()
        Just picArr -> do
            deleteNewsPictures conn (API.newsID editNewsRequest)
            addPicturesToNews conn (API.newsID editNewsRequest) picArr
            return ()