module DataBaseQueries.News where

import Types.Domain.User
import Types.Domain.News
import Types.Domain.Picture
import Types.Domain.Category
import qualified Types.Database.News as DBType
import qualified Types.API.News as API
import qualified Types.Database.Category as DBType
import Database.PostgreSQL.Simple
import qualified Data.Text as T
import Control.Monad (mapM)

parseNewsList :: Connection -> IO [(DBType.News)]
parseNewsList conn = do
    res <- query_ conn "SELECT * FROM news"
    return res

writeNews :: Connection -> News -> IO ()
writeNews conn News {..} = do
    let q = "INSERT INTO news (title, create_date, creator_id, category_id, text_content, is_published) values (?,?,?,?,?,?) RETURNING id"
    [Only newId] <- query conn q (title, createDate, 1 :: Int, (categoryID category), textContent, isPublished) :: IO [Only Int]  --HARD_CODE
    case picturesArray of
        Nothing -> return ()
        Just picArr -> do
            print picArr
            mapM ( \Picture {..} -> do
                let q = "INSERT INTO pictures (base64) values (?) RETURNING id"
                [Only picID] <- query conn q (Only base64) :: IO [Only Int]
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

        execPicturesArray (Just picArr) = do
            execute conn "DELETE * FROM news_pictures WHERE news_id = ?"
                    (Only newsID)
            mapM ( \(Picture b64) -> 
                execute conn "UPDATE news_pictures SET pictures_id = ? WHERE news_id = ?"
                        (b64, newsID)
                ) picArr
        execPicturesArray Nothing = pure []