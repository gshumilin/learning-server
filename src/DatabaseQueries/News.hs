module DatabaseQueries.News where

import qualified Types.Domain.News as Domain
import qualified Types.Domain.Picture as Domain
import qualified Types.Database.News as DBType
import Types.Domain.Environment
import qualified Types.API.News as API
import DatabaseQueries.QueryCreator (makeReadNewsQuery)
import DatabaseQueries.Picture (findPicturesArray)
import DatabaseQueries.User (findUser)
import Endpoints.Categories (getSpecificCategory)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Types
import qualified Data.Text as T
import Data.Time
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
    newsPicturesArray <- findPicturesArray conn newsID
    return $ Domain.News 
        {   newsID = newsID,
            title = title,
            createDate = createDate,
            creator = newsCreator,
            category = newsCategory,
            textContent = textContent,
            picturesArray = newsPicturesArray,
            isPublished = isPublished, 
            numbersOfPictures = numbersOfPictures
        }

writeNews :: Connection -> API.CreateNewsRequest -> IO ()
writeNews conn API.CreateNewsRequest {..} = do
    let newsCreatorID = 1 :: Int
    currTime <- getCurrentTime
    let isPublished = False
    let q = "INSERT INTO news (title, create_date, creator_id, category_id, text_content, is_published) values (?,?,?,?,?,?) RETURNING id"
    [Only newId] <- query conn q (title, currTime, newsCreatorID, categoryID, textContent, isPublished) :: IO [Only Int]
    case picturesArray of
        Nothing -> return ()
        Just picArr -> do
            mapM ( \Domain.Picture {..} -> do
                let q = "INSERT INTO pictures (data,mime) values (?,?) RETURNING id"
                [Only picID] <- query conn q (picData, mime) :: IO [Only Int]
                let q' = "INSERT INTO news_pictures (news_id, picture_id) values (?,?)"
                execute conn q' (newId, picID)            
                ) picArr
            return ()

rewriteNews :: Connection -> API.EditNewsRequest -> IO ()
rewriteNews conn API.EditNewsRequest {..} = do
    editTitle <- execTitle newTitle
    editCategory <- execCategoryID newCategoryID
    editText <- execTextContent newTextContent
    editPictures <- execPicturesArray newPicturesArray
    return ()
    where 
        execTitle (Just tit) = execute conn "UPDATE news SET title = ? WHERE id = ?" (tit, newsID)
        execTitle Nothing = pure 0

        execCategoryID (Just cat) = execute conn "UPDATE news SET category_id = ? WHERE id = ?" (cat, newsID)
        execCategoryID Nothing = pure 0

        execTextContent (Just txt) = execute conn "UPDATE news SET text_content = ? WHERE id = ?" (txt, newsID)
        execTextContent Nothing = pure 0

        execPicturesArray (Just picArr) = undefined
            -- execute conn "DELETE * FROM news_pictures WHERE news_id = ?"
            --         (Only newsID)
            -- mapM ( \(Domain.Picture b64) -> 
            --     execute conn "UPDATE news_pictures SET pictures_id = ? WHERE news_id = ?"
            --             (b64, newsID)
            --     ) picArr
        execPicturesArray Nothing = pure []