module DatabaseQueries.Auth where

import Types.Domain.Environment
import qualified Types.Database.User as Database
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import qualified Data.ByteString.Char8 as BS
import Data.ByteString.Base64
import qualified Data.Text as T
import Data.List (takeWhile, dropWhile)
import Control.Monad.Reader

instance FromRow Bool where
    fromRow = field

instance FromRow Int where
    fromRow = field

authentication :: Connection -> (BS.ByteString, BS.ByteString) -> IO (Either T.Text Database.User)
authentication conn (login, password) = do
    let q = "SELECT * FROM users WHERE login = ? AND password = ?"
    mbRes <- query conn q (login :: BS.ByteString, password :: BS.ByteString)
    case mbRes of
        [] -> return (Left "Authentication fail")
        [user] -> return (Right user)