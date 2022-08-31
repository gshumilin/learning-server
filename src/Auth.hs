module Auth where

import Control.Monad.Reader
import Data.ByteString.Base64 (decodeBase64)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Internal as BS (packChars)
import Data.List (find)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Database.PostgreSQL.Simple (Connection)
import DatabaseQueries.Auth (authentication)
import Hash (passHashBS)
import Log (addLog)
import Network.HTTP.Types (hContentType, status404)
import Network.HTTP.Types.Header
import Network.Wai
import qualified Types.Database.User as Database
import Types.Domain.Environment
import Types.Domain.Log

authFailResponse :: Response
authFailResponse = responseLBS status404 [(hContentType, "text/plain")] "Not found"

authorization :: Connection -> Request -> IO (Either T.Text Database.User)
authorization conn req = do
  case decodeAuthKey =<< findAuthKey req of
    Left err -> pure (Left err)
    Right loginPassword -> authentication conn loginPassword

findAuthKey :: Request -> Either T.Text BS.ByteString
findAuthKey req =
  case snd <$> authHeader of
    Nothing -> Left "There is no authenticate header"
    Just auth -> Right auth
  where
    authHeader = find (\(h, _) -> h == hAuthorization) (requestHeaders req)

decodeAuthKey :: BS.ByteString -> Either T.Text (BS.ByteString, BS.ByteString)
decodeAuthKey base64code = makeAuthTuple <$> decodeBase64 base64code
  where
    makeAuthTuple decodedInfo =
      ( BS.takeWhile (/= ':') decodedInfo,
        passHashBS . BS.tail $ BS.dropWhile (/= ':') decodedInfo
      )
