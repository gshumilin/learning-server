module DatabaseQueries.User where

import qualified Data.ByteString.Char8 as BS
import Database.PostgreSQL.Simple (Connection, Only (..), execute, query, query_)
import qualified Types.Domain.User as Domain (User (..))

readUsers :: Connection -> IO [Domain.User]
readUsers conn = query_ conn "SELECT name,login,password,create_date,is_admin,is_able_to_create_news FROM users"

writeUser :: Connection -> Domain.User -> IO ()
writeUser conn Domain.User {..} = do
  _ <-
    execute
      conn
      "INSERT INTO users (name,login,password,create_date,is_admin,is_able_to_create_news) values (?,?,?,?,?,?)"
      (name, login, password, createDate, isAdmin, isAbleToCreateNews)
  pure ()

findUser :: Connection -> Int -> IO Domain.User
findUser conn userID = do
  res <- query conn "SELECT name,login,password,create_date,is_admin,is_able_to_create_news FROM users WHERE id = ?" $ Only userID
  pure $ head res

findUserIdByLogin :: Connection -> BS.ByteString -> IO (Maybe BS.ByteString)
findUserIdByLogin conn login = do
  res <- query conn "SELECT id FROM users WHERE login = ?" $ Only login
  case res of
    [] -> pure Nothing
    ((Only x) : _) -> pure $ Just (BS.pack . show $ (x :: Int))
