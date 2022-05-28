module DataBaseQueries where

import Types.User
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

getConnection :: IO Connection
getConnection = connectPostgreSQL "host='localhost' port=5432 dbname='learning_server_db' user='learning_server_user' password='pleasedonthackme'" 