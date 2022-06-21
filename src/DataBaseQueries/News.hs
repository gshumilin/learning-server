module DataBaseQueries.News where

import Types.User
import Types.News
import Types.Picture
import Types.Category
import qualified Types.Database.News as DBType
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
                    (title, createDate, creator, category, textContent, picturesArray, isPublished)
    return ()