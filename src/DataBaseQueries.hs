module DataBaseQueries where

import Types.User
import Types.News
import Types.Picture
import Database.PostgreSQL.Simple

parseUsersList :: Connection -> IO [(User)]
parseUsersList conn = do
    res <- query_ conn "SELECT name,login,password,createDate,isAdmin,isAbleToCreateNews FROM userslist"
    return res
    
writeUser :: Connection -> User -> IO ()
writeUser conn User {..} = do
    execute conn "insert into userslist (name,login,password,createDate,isAdmin,isAbleToCreateNews) values (?,?,?,?,?,?)"
                    (name, login, password, createDate, isAdmin, isAbleToCreateNews)
    return ()

parseNewsList :: Connection -> IO [(News)]
parseNewsList conn = do
    res <- query_ conn "SELECT header, createDate, creator, category, textContent, picturesArray, isPublished FROM userslist"
    return res

writeNews :: Connection -> News -> IO ()
writeNews conn News {..} = do
    execute conn "insert into newsList (header, createDate, creator, category, textContent, picturesArray, isPublished) values (?,?,?,?,?,?)"
                    (header, createDate, creator, category, textContent, picturesArray, isPublished)
    return ()

getConnection :: IO Connection
getConnection = connectPostgreSQL "host='localhost' port=5432 dbname='learning_server_db' user='learning_server_user' password='pleasedonthackme'" 