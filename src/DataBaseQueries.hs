module DataBaseQueries where

import Types.User
import Types.News
import qualified Types.Database.News as DBType
import Types.Picture
import Types.Category
import qualified Types.Database.Category as DBType
import Database.PostgreSQL.Simple
import qualified Data.Text as T


getConnection :: IO Connection
getConnection = connectPostgreSQL "host='localhost' port=5432 dbname='learning_server_db' user='learning_server_user' password='pleasedonthackme'" 

parseUsersList :: Connection -> IO [(User)]
parseUsersList conn = do
    res <- query_ conn "SELECT name,login,password,create_date,is_admin,is_able_to_create_news FROM users"
    return res
    
writeUser :: Connection -> User -> IO ()
writeUser conn User {..} = do
    execute conn "insert into users (name,login,password,create_date,is_admin,is_able_to_create_news) values (?,?,?,?,?,?)"
                    (name, login, password, createDate, isAdmin, isAbleToCreateNews)
    return ()

parseNewsList :: Connection -> IO [(DBType.News)]
parseNewsList conn = do
    res <- query_ conn "SELECT * FROM news"
    return res

writeNews :: Connection -> News -> IO ()
writeNews conn News {..} = do
    execute conn "insert into news (title, create_date, creator_id, category_id, text_content, is_published) values (?,?,?,?,?,?)"
                    (title, createDate, creator, category, textContent, picturesArray, isPublished)
    return ()

parseCategoriesList :: Connection -> IO [(DBType.Category)]
parseCategoriesList conn = do
    res <- query_ conn "SELECT * FROM categories"
    return res