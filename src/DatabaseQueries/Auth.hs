module DatabaseQueries.Auth where

import Control.Monad.Reader (ReaderT)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import Database.PostgreSQL.Simple (query)
import qualified Types.DB.User as DB (User (..))
import Types.Domain.Environment (Environment (..))
import Utils.Pool (withPool)

authentication :: (BS.ByteString, BS.ByteString) -> ReaderT Environment IO (Either T.Text DB.User)
authentication (login, password) = do
  let q =
        " SELECT * FROM users \
        \ WHERE login = ? AND password = ?"
  mbRes <- withPool $ \conn -> query conn q (login :: BS.ByteString, password :: BS.ByteString)
  case mbRes of
    [] -> pure (Left "Authentication fail")
    (user : _) -> pure (Right user)
