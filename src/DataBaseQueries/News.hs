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

parseNewsList :: Connection -> IO [(DBType.News)]
parseNewsList conn = do
    res <- query_ conn "SELECT * FROM news"
    return res

writeNews :: Connection -> News -> IO ()
writeNews conn News {..} = do
    execute conn "insert into news (title, create_date, creator_id, category_id, text_content, is_published) values (?,?,?,?,?,?)"
                    (title, createDate, 1 :: Int, (categoryID category), textContent, isPublished)  --HARD_CODE
    return ()

rewriteNews :: Connection -> API.EditNewsRequest -> IO ()
rewriteNews conn API.EditNewsRequest {..} = do
    execute conn "UPDATE news SET title = ?, category_id = ?, text_content = ? WHERE id = ?"
                    (title, newsID, textContent, newsID)  --HARD_CODE
    return ()