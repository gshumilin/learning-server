module DatabaseQueries.User where

import Types.Domain.User
import Database.PostgreSQL.Simple

parseUsersList :: Connection -> IO [(User)]
parseUsersList conn = do
    res <- query_ conn "SELECT name,login,password,create_date,is_admin,is_able_to_create_news FROM users"
    return res
    
writeUser :: Connection -> User -> IO ()
writeUser conn User {..} = do
    execute conn "INSERT INTO users (name,login,password,create_date,is_admin,is_able_to_create_news) values (?,?,?,?,?,?)"
                    (name, login, password, createDate, isAdmin, isAbleToCreateNews)
    return ()

findUser :: Connection -> Int -> IO (User)
findUser conn userID = do
    res <- query conn "SELECT * FROM users WHERE id = ?" $ Only userID
    return $ head res