module Routing where

import Control.Monad.Reader
import Endpoints.CreateCategory (createCategory)
import Endpoints.EditCategory (editCategory)
import Endpoints.EditNews (editNews)
import Endpoints.GetCategories (getCategories)
import qualified Endpoints.News
import qualified Endpoints.Picture
import qualified Endpoints.User
import Log (addLog)
import Network.Wai
import Types.DB.User (isAbleToCreateNews, isAdmin)
import Types.Domain.Environment
import Types.Domain.Log
import Utils (withAuth, withAuthAndParsedRequest)

application :: Request -> (Response -> IO ResponseReceived) -> ReaderT Environment IO ResponseReceived
application req respond = do
  addLog DEBUG ("----- got request:\n" ++ show req ++ "\n")
  addLog DEBUG ("----- method : " ++ show (rawPathInfo req) ++ "\n")
  case rawPathInfo req of
    "/getUsers" -> do
      res <- Endpoints.User.getUsers
      lift $ respond res
    "/createUser" -> do
      res <- withAuth isAdmin Endpoints.User.createUser req
      lift $ respond res
    "/getNews" -> do
      res <- Endpoints.News.getNews req
      lift $ respond res
    "/createNews" -> do
      res <- withAuth isAbleToCreateNews Endpoints.News.createNews req
      lift $ respond res
    "/editNews" -> do
      res <- withAuthAndParsedRequest editNews req
      lift $ respond res
    "/getCategories" -> do
      res <- getCategories
      lift $ respond res
    "/createCategory" -> do
      res <- withAuthAndParsedRequest createCategory req
      lift $ respond res
    "/editCategory" -> do
      res <- withAuthAndParsedRequest editCategory req
      lift $ respond res
    "/getPicture" -> do
      res <- Endpoints.Picture.getPicture req
      lift $ respond res
    "/putPicture" -> do
      res <- Endpoints.Picture.putPicture req
      lift $ respond res
    _ -> error "Unknown method"
