module DatabaseQueries.Auth where

import Control.Monad.Reader
import Data.ByteString.Base64
import qualified Data.ByteString.Char8 as BS
import Data.List (dropWhile, takeWhile)
import qualified Data.Text as T
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import qualified Types.Database.User as Database
import Types.Domain.Environment

instance FromRow Bool where
  fromRow = field

instance FromRow Int where
  fromRow = field

authentication :: Connection -> (BS.ByteString, BS.ByteString) -> IO (Either T.Text Database.User)
authentication conn (login, password) = do
  let q = "SELECT * FROM users WHERE login = ? AND password = ?"
  putStrLn $ show login ++ " = " ++ show password
  mbRes <- query conn q (login :: BS.ByteString, password :: BS.ByteString)
  case mbRes of
    [] -> pure (Left "Authentication fail")
    [user] -> pure (Right user)
