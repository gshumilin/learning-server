module Auth where

import Data.ByteString.Base64 (decodeBase64)
import qualified Data.ByteString.Char8 as BS
import Data.List (find)
import qualified Data.Text as T
import Database.PostgreSQL.Simple (Connection)
import DatabaseQueries.Auth (authentication)
import Hash (passHashBS)
import Network.HTTP.Types (hAuthorization, hContentType, status404)
import Network.Wai (Request, Response, requestHeaders, responseLBS)
import qualified Types.DB.User as DB

authFailResponse :: Response
authFailResponse = responseLBS status404 [(hContentType, "text/plain")] "Not found"

authorization :: Connection -> Request -> IO (Either T.Text DB.User)
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
