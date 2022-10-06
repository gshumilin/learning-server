module Auth where

import Control.Monad.Reader (ReaderT)
import Data.ByteString.Base64 (decodeBase64)
import qualified Data.ByteString.Char8 as BS
import Data.List (find)
import qualified Data.Text as T
import DatabaseQueries.Auth (authentication)
import Hash (passHashBS)
import Network.HTTP.Types (hAuthorization)
import Network.Wai (Request, requestHeaders)
import qualified Types.DB.User as DB
import Types.Domain.Environment (Environment (..))

authorization :: Request -> ReaderT Environment IO (Either T.Text DB.User)
authorization req = do
  case decodeAuthKey =<< findAuthKey req of
    Left err -> pure (Left err)
    Right loginPassword -> authentication loginPassword

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
