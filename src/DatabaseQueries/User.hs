module DatabaseQueries.User where

import qualified Data.Text as T
import Database.PostgreSQL.Simple (Connection, Only (..), query, query_)
import qualified Types.API.User as API (CreateUserRequest (..))
import qualified Types.Domain.User as Domain (User (..))

readUsers :: Connection -> IO [Domain.User]
readUsers conn =
  let q =
        " SELECT name, login, password, create_date, is_admin, is_able_to_create_news \
        \ FROM users"
   in query_ conn q

writeUser :: Connection -> API.CreateUserRequest -> IO Int
writeUser conn API.CreateUserRequest {..} = do
  let q =
        " INSERT INTO users \
        \ (name, login, password, is_admin, is_able_to_create_news) \
        \ VALUES (?,?,?,?,?) \
        \ RETURNING id"
  [Only resId] <- query conn q (reqName, reqLogin, reqPassword, reqIsAdmin, reqIsAbleToCreateNews)
  pure resId

findUser :: Connection -> Int -> IO Domain.User
findUser conn userId = do
  let q =
        " SELECT name, login, password, create_date, is_admin, is_able_to_create_news \
        \ FROM users WHERE id = ?"
  res <- query conn q $ Only userId
  pure $ head res

findUserIdByLogin :: Connection -> T.Text -> IO (Maybe Int)
findUserIdByLogin conn login = do
  res <- query conn "SELECT id FROM users WHERE login = ?" $ Only login
  case res of
    [] -> pure Nothing
    ((Only x) : _) -> pure $ Just ((x :: Int))
