module DataBaseQueries where

import Types.User
import Types.News
import Types.Picture
import Types.Category
import Database.PostgreSQL.Simple
import qualified Data.Text as T


getConnection :: IO Connection
getConnection = connectPostgreSQL "host='localhost' port=5432 dbname='learning_server_db' user='learning_server_user' password='pleasedonthackme'" 

parseUsersList :: Connection -> IO [(User)]
parseUsersList conn = do
    res <- query_ conn "SELECT name,login,password,createDate,isAdmin,isAbleToCreateNews FROM users"
    return res
    
writeUser :: Connection -> User -> IO ()
writeUser conn User {..} = do
    execute conn "insert into users (name,login,password,createDate,isAdmin,isAbleToCreateNews) values (?,?,?,?,?,?)"
                    (name, login, password, createDate, isAdmin, isAbleToCreateNews)
    return ()

parseNewsList :: Connection -> IO [(News)]
parseNewsList conn = do
    res <- query_ conn "SELECT header, createDate, creator, category, textContent, picturesArray, isPublished FROM users"
    return res

writeNews :: Connection -> News -> IO ()
writeNews conn News {..} = do
    execute conn "insert into news (header, createDate, creator, category, textContent, picturesArray, isPublished) values (?,?,?,?,?,?)"
                    (header, createDate, creator, category, textContent, picturesArray, isPublished)
    return ()

parseCategoriesList :: Connection -> IO [(Category)]
parseCategoriesList = undefined