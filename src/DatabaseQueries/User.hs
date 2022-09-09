module DatabaseQueries.User where

import qualified Data.Text as T
import Database.PostgreSQL.Simple (Connection, Only (..), execute, query, query_)
import qualified Types.API.User as API (CreateUserRequest (..))
import qualified Types.Domain.User as Domain (User (..))

readUsers :: Connection -> IO [Domain.User]
readUsers conn = query_ conn "SELECT name,login,password,create_date,is_admin,is_able_to_create_news FROM users"

writeUser :: Connection -> API.CreateUserRequest -> IO ()
writeUser conn API.CreateUserRequest {..} = do
  _ <-
    execute
      conn
      "INSERT INTO users (name,login,password,create_date,is_admin,is_able_to_create_news) values (?,?,?,?,?,?)"
      (reqName, reqLogin, reqPassword, reqIsAdmin, reqIsAbleToCreateNews)
  pure ()

findUser :: Connection -> Int -> IO Domain.User
findUser conn userId = do
  res <- query conn "SELECT name,login,password,create_date,is_admin,is_able_to_create_news FROM users WHERE id = ?" $ Only userId
  pure $ head res

findUserIdByLogin :: Connection -> T.Text -> IO (Maybe Int)
findUserIdByLogin conn login = do
  res <- query conn "SELECT id FROM users WHERE login = ?" $ Only login
  case res of
    [] -> pure Nothing
    ((Only x) : _) -> pure $ Just ((x :: Int))
