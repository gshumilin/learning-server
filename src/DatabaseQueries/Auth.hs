module DatabaseQueries.Auth where

import Auth
import Types.Domain.Environment
import qualified Types.Database.User as DBType
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

authentication :: (BS.ByteString, BS.ByteString) -> IO (Either T.Text DBType.User)
authentication (login, password) = do
    let q = "SELECT * FROM users WHERE login = ? AND password = ?"
    mbRes <- query conn q (login :: BS.ByteString, password :: BS.ByteString)
    case mbRes of
        [] -> return (Left "Authentication fail")
        [user] -> return (Right user)

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

getUserIdWithAuth :: Connection -> Either T.Text (BS.ByteString, BS.ByteString) -> IO (Maybe Int)
getUserIdWithAuth conn (Left _) = return Nothing
getUserIdWithAuth conn (Right (login, password)) = do
    let q = "SELECT id FROM users WHERE login = ? AND password = ?;"
    resArr <- query conn q (login :: BS.ByteString, password :: BS.ByteString)
    case resArr of
        [] -> return Nothing
        [intVal] -> return $ Just intVal