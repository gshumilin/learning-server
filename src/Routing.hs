module Routing where

import Control.Monad.Reader (ReaderT, lift)
import Endpoints.CreateCategory (createCategory)
import Endpoints.CreateNews (createNews)
import Endpoints.CreateUser (createUser)
import Endpoints.EditCategory (editCategory)
import Endpoints.EditNews (editNews)
import Endpoints.GetCategories (getCategories)
import Endpoints.GetNews (getNews)
import Endpoints.GetUser (getUsers)
import Endpoints.Picture (getPicture, putPicture)
import Log (addLog)
import Network.Wai (Request, Response, ResponseReceived, rawPathInfo)
import Types.Domain.Environment (Environment (..))
import Types.Domain.Log (LogLvl (..))
import Utils (withAuthAndParsedRequest)

application :: Request -> (Response -> IO ResponseReceived) -> ReaderT Environment IO ResponseReceived
application req respond = do
  addLog DEBUG ("----- got request:\n" ++ show req ++ "\n")
  addLog DEBUG ("----- method : " ++ show (rawPathInfo req) ++ "\n")
  case rawPathInfo req of
    "/getUsers" -> do
      res <- getUsers
      lift $ respond res
    "/createUser" -> do
      res <- withAuthAndParsedRequest createUser req
      lift $ respond res
    "/getNews" -> do
      res <- getNews req
      lift $ respond res
    "/createNews" -> do
      res <- withAuthAndParsedRequest createNews req
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
      res <- getPicture req
      lift $ respond res
    "/putPicture" -> do
      res <- putPicture req
      lift $ respond res
    _ -> error "Unknown method"
