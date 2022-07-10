module Routing where

import Types.Domain.User
import Types.Domain.Environment
import qualified Endpoints.User
import qualified Endpoints.News
import qualified Endpoints.Picture
import Endpoints.Categories
import DataBaseQueries.GetConnection (getConnection)
import Network.Wai
import qualified Data.ByteString.Char8 as BS
import Control.Monad.Reader
import Data.List (find)
import Text.Read (readMaybe)

application :: Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
application req respond = do
    env <- parseEnvironment
    runReaderT (routing req respond) env

routing :: Request -> (Response -> IO ResponseReceived) -> ReaderT Environment IO ResponseReceived
routing req respond = do
    let reqPath = rawPathInfo req
    case reqPath of
        "/getUsersList" -> do
            res <- Endpoints.User.getUsersList
            lift $ respond res
        "/createUser"   -> do
            res <- Endpoints.User.createUser req
            lift $ respond res
        "/getNewsList"  -> do
            res <- Endpoints.News.getNewsList
            lift $ respond res
        "/createNews"   -> do
            res <- Endpoints.News.createNews req
            lift $ respond res    
        "/editNews"     -> do
            res <- Endpoints.News.editNews req
            lift $ respond res
        "/getCategoriesList" -> do
            res <- Endpoints.Categories.getCategoriesList    
            lift $ respond res
        "/createCategory" -> do
            res <- Endpoints.Categories.createCategory req
            lift $ respond res
        "/editCategory"     -> do
            res <- Endpoints.Categories.editCategory req
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
        _               -> error "Unknown method"

parseEnvironment :: IO (Environment)
parseEnvironment = do
    conn <- getConnection
    return (Environment conn)
