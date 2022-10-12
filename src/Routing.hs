module Routing where

import Control.Exception (try)
import Control.Exception.Base (SomeException)
import Control.Monad.Reader (ReaderT, ask, lift, runReaderT)
import qualified Data.Text as T (pack)
import Endpoints.CreateCategory (createCategory)
import Endpoints.CreateNews (createNews)
import Endpoints.CreateUser (createUser)
import Endpoints.EditCategory (editCategory)
import Endpoints.EditNews (editNews)
import Endpoints.GetCategories (getCategories)
import Endpoints.GetNews (getNews)
import Endpoints.GetUser (getUsers)
import Endpoints.Picture (getPicture)
import Log (addLog)
import Network.HTTP.Types (hContentType, status404, status500)
import Network.Wai (Response, ResponseReceived, responseLBS)
import Network.Wai.Internal (Request (..))
import Types.Domain.Environment (Environment (..))
import Types.Domain.Log (LogLvl (..))
import Utils.Req (withAuthAndParsedRequest)

application :: Request -> (Response -> IO ResponseReceived) -> ReaderT Environment IO ResponseReceived
application req respond = do
  env <- ask
  tryRes <- lift $ try (runReaderT (routing req respond) env) :: ReaderT Environment IO (Either SomeException ResponseReceived)
  case tryRes of
    Left err -> do
      addLog RELEASE $ "FATAL ERROR: " <> T.pack (show err)
      lift . respond $ responseLBS status500 [(hContentType, "text/plain")] "500 Internal Server Error\n"
    Right res -> pure res

routing :: Request -> (Response -> IO ResponseReceived) -> ReaderT Environment IO ResponseReceived
routing req@Request {..} respond = do
  addLog DEBUG ("----- got request:\n" <> T.pack (show req))
  addLog DEBUG ("----- method : " <> T.pack (show requestMethod))
  addLog DEBUG ("----- endpoint : " <> T.pack (show rawPathInfo))
  case requestMethod of
    "GET" ->
      case rawPathInfo of
        "/users" -> do
          res <- getUsers
          lift $ respond res
        "/news" -> do
          res <- getNews req
          lift $ respond res
        "/categories" -> do
          res <- getCategories
          lift $ respond res
        "/picture" -> do
          res <- getPicture req
          lift $ respond res
        _ -> do
          addLog WARNING "Unknown endpoint"
          lift . respond $ responseLBS status404 [(hContentType, "text/plain")] "Unknown endpoint"
    "POST" ->
      case rawPathInfo of
        "/user" -> do
          res <- withAuthAndParsedRequest createUser req
          lift $ respond res
        "/news" -> do
          res <- withAuthAndParsedRequest createNews req
          lift $ respond res
        "/category" -> do
          res <- withAuthAndParsedRequest createCategory req
          lift $ respond res
        _ -> do
          addLog WARNING "Unknown endpoint"
          lift . respond $ responseLBS status404 [(hContentType, "text/plain")] "Unknown endpoint"
    "PUT" ->
      case rawPathInfo of
        "/news" -> do
          res <- withAuthAndParsedRequest editNews req
          lift $ respond res
        "/category" -> do
          res <- withAuthAndParsedRequest editCategory req
          lift $ respond res
        _ -> do
          addLog WARNING "Unknown endpoint"
          lift . respond $ responseLBS status404 [(hContentType, "text/plain")] "Unknown endpoint"
    _ -> do
      addLog WARNING "Unknown method"
      lift . respond $ responseLBS status404 [(hContentType, "text/plain")] "Unknown method"
