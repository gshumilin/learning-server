module Auth where

import Types.Domain.Environment
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
import qualified Data.ByteString.Lazy.Internal as BS (packChars)

authFailResponse :: Response
authFailResponse = responseLBS status404 [(hContentType, "text/plain")] $ "Not found"

authDecodeFailResponse :: T.Text -> Response
authDecodeFailResponse decodeErr = responseLBS status404 [(hContentType, "text/plain")] . BS.packChars . T.unpack $ decodeErr

findAuthKey :: Request -> Maybe BS.ByteString
findAuthKey req = 
    case authHeader of     
        Nothing -> Nothing
        Just (_, authKey) -> return authKey
    where 
        authHeader = (find (\(h,_) -> h == hAuthorization) (requestHeaders req))

withAuth :: (BS.ByteString -> ReaderT Environment IO (Either T.Text Bool))
            -> (Request -> ReaderT Environment IO Response) 
            -> Request 
            -> ReaderT Environment IO Response
withAuth checkFunc endpointFunc req = do
    case findAuthKey req  of
        Nothing -> return authFailResponse
        Just authKey -> do
            checkAuthResult <- checkFunc authKey
            case checkAuthResult of
                Left err -> return $ authDecodeFailResponse err
                Right False -> return $ authFailResponse
                Right True -> do
                    res <- endpointFunc req
                    return res

decodeAuthKey :: BS.ByteString -> Either T.Text (BS.ByteString, BS.ByteString)
decodeAuthKey base64code = 
    case decodeBase64 base64code of
        Left err -> Left err
        Right decoded -> Right (BS.takeWhile (/=':') decoded, BS.tail $ BS.dropWhile (/= ':') decoded )

getUserNameFromAuth :: Request -> Maybe BS.ByteString
getUserNameFromAuth req = 
    case findAuthKey req of
        Nothing -> Nothing
        Just header -> undefined