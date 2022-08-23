module Routing where

import Auth
import Types.Domain.User
import Types.Database.User (isAdmin, isAbleToCreateNews)
import Types.Domain.Environment
import qualified Endpoints.User
import qualified Endpoints.News
import qualified Endpoints.Picture
import Endpoints.Categories
import DatabaseQueries.Auth
import Network.Wai
import Network.HTTP.Types (hContentType, status404)
import Network.HTTP.Types.Header
import qualified Data.Text as T
import Control.Monad.Reader

routing :: Request -> (Response -> IO ResponseReceived) -> ReaderT Environment IO ResponseReceived
routing req respond = do
    lift . putStrLn $ "----- got request:\n" ++ (show req) ++ "\n"              --log
    case rawPathInfo req of
        "/getUsersList" -> do
            res <- Endpoints.User.getUsersList
            lift $ respond res
        "/createUser"   -> do
            res <- withAuth isAdmin Endpoints.User.createUser req
            lift $ respond res
        "/getNews"  -> do
            res <- Endpoints.News.getNews req
            lift $ respond res
        "/createNews"   -> do
            res <- withAuth isAbleToCreateNews Endpoints.News.createNews req
            lift $ respond res    
        "/editNews"     -> do
            res <- Endpoints.News.editNews req
            lift $ respond res
        "/getCategoriesList" -> do
            res <- Endpoints.Categories.getCategoriesList    
            lift $ respond res
        "/createCategory" -> do
            res <- withAuth isAdmin Endpoints.Categories.createCategory req
            lift $ respond res
        "/editCategory"     -> do
            res <- withAuth isAdmin Endpoints.Categories.editCategory req
            lift $ respond res
        "/getPicture"     -> do
            res <- Endpoints.Picture.getPicture req
            lift $ respond res
        "/putPicture"     -> do
            res <- Endpoints.Picture.putPicture req
            lift $ respond res
        _               -> error "Unknown method"