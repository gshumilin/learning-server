module Routing where

import Types.Domain.User
import Types.Domain.Environment
import qualified Endpoints.User
import qualified Endpoints.News
import qualified Endpoints.Picture
import Endpoints.Categories
import DataBaseQueries.GetConnection (getConnection)
import DataBaseQueries.Auth (checkIsAdmin, checkIsAbleToCreateNews)
import Network.Wai
import Network.HTTP.Types (hContentType, status404)
import Network.HTTP.Types.Header
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Internal as BS (packChars)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Control.Monad.Reader
import Data.List (find)
import Text.Read (readMaybe)
import Database.PostgreSQL.Simple (Connection)

application :: Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
application req respond = do
    env <- parseEnvironment
    runReaderT (routing req respond) env

routing :: Request -> (Response -> IO ResponseReceived) -> ReaderT Environment IO ResponseReceived
routing req respond = do
    let reqPath = rawPathInfo req
    let reqHeaders = requestHeaders req
    conn <- asks dbConnection
    case reqPath of
        "/getUsersList" -> do
            res <- Endpoints.User.getUsersList
            lift $ respond res
        "/createUser"   -> do
            res <- withAuth checkIsAdmin Endpoints.User.createUser req
            lift $ respond res
        "/getNewsList"  -> do
            res <- Endpoints.News.getNewsList
            lift $ respond res
        "/createNews"   -> do
            res <- withAuth checkIsAbleToCreateNews Endpoints.News.createNews req
            lift $ respond res    
        "/editNews"     -> do
            res <- Endpoints.News.editNews req
            lift $ respond res
        "/getCategoriesList" -> do
            res <- Endpoints.Categories.getCategoriesList    
            lift $ respond res
        "/createCategory" -> do
            res <- withAuth checkIsAdmin Endpoints.Categories.createCategory req
            lift $ respond res
        "/editCategory"     -> do
            res <- withAuth checkIsAdmin Endpoints.Categories.editCategory req
            lift $ respond res
        "/getPicture"     -> do
            let idPole = find (\(k,v) -> k == "id") $ queryString req
            case idPole of
                Nothing -> error "'id' parameter not specified"
                Just (_, Nothing) -> error "empty value of the id parameter"
                Just (_, Just bsPicID) -> do
                    let mbPicID = readMaybe $ BS.unpack bsPicID :: Maybe Int
                    case mbPicID of
                        Nothing -> do
                            error "invalid id parameter value"
                        Just picID -> do 
                            res <- Endpoints.Picture.getPicture picID
                            lift $ respond res
        "sdsdsdsdsdsd"  -> do
            res <- withAuth checkIsAdmin (Endpoints.Categories.createCategory) req
            lift $ respond res
        _               -> error "Unknown method"

parseEnvironment :: IO (Environment)
parseEnvironment = do
    conn <- getConnection
    return (Environment conn)

authFailResponse :: Response
authFailResponse = responseLBS status404 [(hContentType, "text/plain")] $ "Not found"

authDecodeFailResponse :: T.Text -> Response
authDecodeFailResponse decodeErr = responseLBS status404 [(hContentType, "text/plain")] . BS.packChars . T.unpack $ decodeErr

findAuthHeader :: Request -> Maybe BS.ByteString
findAuthHeader req = 
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
    case findAuthHeader req  of
        Nothing -> return authFailResponse
        Just authKey -> do
            checkAuthResult <- checkFunc authKey
            case checkAuthResult of
                Left err -> return $ authDecodeFailResponse err
                Right False -> return $ authFailResponse
                Right True -> do
                    res <- endpointFunc req
                    return res