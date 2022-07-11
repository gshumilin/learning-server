module DataBaseQueries.Auth where

import Types.Domain.Environment
import Database.PostgreSQL.Simple
import qualified Data.ByteString.Char8 as BS
import Database.PostgreSQL.Simple.FromRow
import Data.ByteString.Base64
import qualified Data.Text as T
import Data.List (takeWhile, dropWhile)
import Control.Monad.Reader

instance FromRow Bool where
    fromRow = field

checkIsAdmin :: BS.ByteString -> ReaderT Environment IO (Either T.Text Bool)
checkIsAdmin key = do
    conn <- asks dbConnection 
    case decodeAuthKey key of
        Left err -> return $ Left err
        Right (login, password) -> do
            let q = "SELECT is_admin FROM users WHERE login = ? AND password = ?;"
            resArr <- lift $ query conn q (login :: BS.ByteString, password :: BS.ByteString)
            case resArr of
                [] -> return (Right False)
                [boolVal] -> return (Right boolVal)

checkIsAbleToCreateNews :: BS.ByteString -> ReaderT Environment IO (Either T.Text Bool)
checkIsAbleToCreateNews key = do
    conn <- asks dbConnection
    case decodeAuthKey key of
        Left err -> return $ Left err
        Right (login, password) -> do
            let q = "SELECT is_able_to_create_news FROM users WHERE login = ? AND password = ?;"
            resArr <- lift $ query conn q (login :: BS.ByteString, password :: BS.ByteString)
            case resArr of
                [] -> return (Right False)
                [boolVal] -> return (Right boolVal)

decodeAuthKey :: BS.ByteString -> Either T.Text (BS.ByteString, BS.ByteString)
decodeAuthKey base65code = 
    case decodeBase64 base65code of
        Left err -> Left err
        Right decoded -> Right (BS.takeWhile (/=':') decoded, BS.tail $ BS.dropWhile (/= ':') decoded )