module Utils where

import Auth (authFailResponse, authorization)
import Control.Monad.Reader (ReaderT, asks, lift)
import Data.Aeson (FromJSON, decodeStrict)
import Log (addLog)
import Network.HTTP.Types (hContentType, status400, status404)
import Network.Wai (Request, Response, getRequestBodyChunk, responseLBS)
import qualified Types.DB.User as DB (User (..))
import Types.Domain.Environment (Environment (..))
import Types.Domain.Log (LogLvl (..))

withParsedRequest :: FromJSON a => (a -> ReaderT Environment IO Response) -> Request -> ReaderT Environment IO Response
withParsedRequest f req = do
  rawJSON <- lift $ getRequestBodyChunk req
  let decodedReq = decodeStrict rawJSON
  case decodedReq of
    Nothing -> do
      addLog WARNING "Invalid JSON"
      pure $ responseLBS status400 [(hContentType, "text/plain")] "Bad Request: Invalid JSON\n"
    Just parsedReq -> f parsedReq

withAuthAndParsedRequest ::
  FromJSON a =>
  (DB.User -> a -> ReaderT Environment IO Response) ->
  Request ->
  ReaderT Environment IO Response
withAuthAndParsedRequest f req = do
  conn <- asks dbConnection
  eiInvoker <- lift $ authorization conn req
  case eiInvoker of
    Left _ -> do
      addLog WARNING "Authorization fail"
      pure $ responseLBS status404 [(hContentType, "text/plain")] "404 : Not Found"
    Right invoker -> do
      rawJSON <- lift $ getRequestBodyChunk req
      case decodeStrict rawJSON of
        Nothing -> do
          addLog WARNING "Invalid JSON"
          pure $ responseLBS status400 [(hContentType, "text/plain")] "Bad Request: Invalid JSON"
        Just parsedReq -> f invoker parsedReq

withAuth ::
  (DB.User -> Bool) ->
  (Request -> ReaderT Environment IO Response) ->
  Request ->
  ReaderT Environment IO Response
withAuth isFunc endpointFunc req = do
  conn <- asks dbConnection
  auth <- lift $ authorization conn req
  case auth of
    Left err -> do
      addLog WARNING $ "----- There is authError: \"" ++ show err ++ "\"\n"
      pure authFailResponse
    Right user ->
      if isFunc user
        then endpointFunc req
        else do
          addLog WARNING "----- There is authError: \"authentication fail\" \n"
          pure authFailResponse
