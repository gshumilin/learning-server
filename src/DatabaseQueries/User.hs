module DatabaseQueries.User where

import Control.Monad (void)
import Control.Monad.Reader (ReaderT)
import qualified Data.Text as T
import Database.PostgreSQL.Simple (Only (..), execute, query, query_)
import qualified Types.API.User as API (CreateUserRequest (..))
import Types.Domain.Environment (Environment (..))
import qualified Types.Domain.User as Domain (User (..))
import Utils.Pool (withPool)

readUsers :: ReaderT Environment IO [Domain.User]
readUsers =
  let q =
        " SELECT name, login, password, create_date, is_admin, is_able_to_create_news \
        \ FROM users"
   in withPool $ \conn -> query_ conn q

writeUser :: API.CreateUserRequest -> ReaderT Environment IO ()
writeUser API.CreateUserRequest {..} = do
  let q =
        " INSERT INTO users \
        \ (name, login, password, is_admin, is_able_to_create_news) \
        \ VALUES (?,?,?,?,?)"
  void . withPool $ \conn -> execute conn q (reqName, reqLogin, reqPassword, reqIsAdmin, reqIsAbleToCreateNews)

findUser :: Int -> ReaderT Environment IO Domain.User
findUser userId = do
  let q =
        " SELECT name, login, password, create_date, is_admin, is_able_to_create_news \
        \ FROM users WHERE id = ?"
  (res : _) <- withPool $ \conn -> query conn q $ Only userId
  pure res

findUserIdByLogin :: T.Text -> ReaderT Environment IO (Maybe Int)
findUserIdByLogin login = do
  res <- withPool $ \conn -> query conn "SELECT id FROM users WHERE login = ?" $ Only login
  case res of
    [] -> pure Nothing
    ((Only x) : _) -> pure $ Just (x :: Int)
