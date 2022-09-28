module DatabaseQueries.Auth where

import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import Database.PostgreSQL.Simple (Connection, query)
import qualified Types.DB.User as DB (User (..))

authentication :: Connection -> (BS.ByteString, BS.ByteString) -> IO (Either T.Text DB.User)
authentication conn (login, password) = do
  let q =
        " SELECT * FROM users \
        \ WHERE login = ? AND password = ?"
  mbRes <- query conn q (login :: BS.ByteString, password :: BS.ByteString)
  case mbRes of
    [] -> pure (Left "Authentication fail")
    (user : _) -> pure (Right user)
