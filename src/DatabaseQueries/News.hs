module DatabaseQueries.News where

import DatabaseQueries.GetConnection
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
    conn <- asks dbConnection
    mbQuery <- lift $ makeReadNewsQuery conn req
    case mbQuery of 
        Nothing -> return []
        Just q -> do
            let (Query bsQ) = q
            lift . BS.putStrLn $ "----- made this psql-request: \n\"" <> bsQ <> "\"\n" --log
            dbNews <- lift $ query_ conn q :: ReaderT Environment IO [DBType.News]
            lift . putStrLn $ "----- got this psql News List: \"" ++ (show dbNews) ++ "\"\n" --log
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

-- rewriteNews :: Connection -> API.EditNewsRequest -> IO ()
-- rewriteNews conn API.EditNewsRequest {..} = do undefined
    -- editTitle <- execTitle newTitle
    -- editCategory <- execCategoryID newCategoryID
    -- editText <- execTextContent newTextContent
    -- editPictures <- execPicturesArray newPictures
    -- return ()
    -- where 
    --     execTitle (Just tit) = execute conn "UPDATE news SET title = ? WHERE id = ?" (tit, newsID)
    --     execTitle Nothing = pure 0

    --     execCategoryID (Just cat) = execute conn "UPDATE news SET category_id = ? WHERE id = ?" (cat, newsID)
    --     execCategoryID Nothing = pure 0

    --     execTextContent (Just txt) = execute conn "UPDATE news SET text_content = ? WHERE id = ?" (txt, newsID)
    --     execTextContent Nothing = pure 0

    --     execPicturesArray (Just picArr) = do
    --         execute conn "DELETE * FROM pictures WHERE id = ?"
    --                 (Only newsID)
    --         execute conn "DELETE * FROM news_pictures WHERE news_id = ?"
    --                 (Only newsID)
    --         mapM ( \(Domain.Picture {..}) -> 
    --             execute conn "INSERT INTO news_pictures SET pictures_id = ? WHERE news_id = ?"
    --                     (mime, picData)
    --             ) picArr
    --     execPicturesArray Nothing = pure []

-- updateDbNewsType :: API.EditNewsRequest -> DBType.EditedNewsFields -> DBType.News
-- updateDbNewsType editNewsRequest DBType.EditedNewsFields {..} = 
--     DBType.News {   DBType.newsID = DBType.oldNewsID,
--                     DBType.title = fromMaybe DBType.oldTitle (API.newTitle editNewsRequest),
--                     DBType.textContent = fromMaybe DBType.oldTextContent (API.newTextContent editNewsRequest),
--                     DBType.creatorID = DBType.oldCreatorID,
--                     DBType.categoryID = fromMaybe DBType.oldCategoryID (API.newCategoryID editNewsRequest)
--                 }