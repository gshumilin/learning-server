module Auth where

import Types.Domain.Environment
import qualified Types.Database.User as Database
import DatabaseQueries.Auth (authentication)
import Database.PostgreSQL.Simple (Connection)
import qualified Data.Text as T
import Network.Wai
import Network.HTTP.Types (hContentType, status404)
import Network.HTTP.Types.Header
import Data.List (find)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Internal as BS (packChars)
import Data.ByteString.Base64 (decodeBase64)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Control.Monad.Reader

authFailResponse :: Response
authFailResponse = responseLBS status404 [(hContentType, "text/plain")] $ "Not found"

authDecodeFailResponse :: T.Text -> Response
authDecodeFailResponse decodeErr = responseLBS status404 [(hContentType, "text/plain")] . BS.packChars . T.unpack $ decodeErr

authorization :: Request -> IO (Either T.Text Database.User)
authorization req = authentication =<< decodeAuthKey <$> findAuthKey req

findAuthKey :: Request -> Either T.Text BS.ByteString
findAuthKey req = 
    case snd <$> authHeader of
        Nothing -> Left "There is no authenticate header"
        Just auth -> Right auth
    where 
        authHeader = (find (\(h,_) -> h == hAuthorization) (requestHeaders req))

decodeAuthKey :: BS.ByteString -> Either T.Text (BS.ByteString, BS.ByteString)
decodeAuthKey base64code = makeAuthTuple <$> decodeBase64 base64code
    where 
        makeAuthTuple decodedInfo = (BS.takeWhile (/=':') decodedInfo, BS.tail $ BS.dropWhile (/= ':') decodedInfo)

withAuth :: (BS.ByteString -> ReaderT Environment IO (Either T.Text Bool))
            -> (Request -> ReaderT Environment IO Response) 
            -> Request 
            -> ReaderT Environment IO Response
withAuth checkFunc endpointFunc req = do
    case findAuthKey req  of
        Left err -> return authFailResponse
        Right authKey -> do
            checkAuthResult <- checkFunc authKey
            case checkAuthResult of
                Left err -> return $ authDecodeFailResponse err
                Right False -> return $ authFailResponse
                Right True -> do
                    res <- endpointFunc req
                    return res