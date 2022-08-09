module DatabaseQueries.User where

import qualified Types.Domain.User as Domain
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import qualified Data.ByteString.Char8 as BS

instance FromRow Int where
    fromRow = field

parseUsersList :: Connection -> IO [(Domain.User)]
parseUsersList conn = do
    res <- query_ conn "SELECT name,login,password,create_date,is_admin,is_able_to_create_news FROM users"
    return res
    
writeUser :: Connection -> Domain.User -> IO ()
writeUser conn Domain.User {..} = do
    execute conn "INSERT INTO users (name,login,password,create_date,is_admin,is_able_to_create_news) values (?,?,?,?,?,?)"
                    (name, login, password, createDate, isAdmin, isAbleToCreateNews)
    return ()

findUser :: Connection -> Int -> IO (Domain.User)
findUser conn userID = do
    res <- query conn "SELECT name,login,password,create_date,is_admin,is_able_to_create_news FROM users WHERE id = ?" $ Only userID
    return $ head res

findUserIdByLogin :: Connection -> BS.ByteString -> IO (Maybe BS.ByteString)
findUserIdByLogin conn login = do
    res <- query conn "SELECT id FROM users WHERE login = ?" $ Only login
    putStrLn $ "----- got this from db wher parse UserId: \"" ++ (show res) ++ "\"\n" --log
    case res of 
        [] -> return Nothing
        (x:xs) -> return $ Just (BS.pack . show $ ( x:: Int))