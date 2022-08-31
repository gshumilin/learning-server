module Utils where

import DatabaseQueries.Auth (authentication)
import Types.Domain.Log (LogLvl(..))
import Log (addLog)
import Auth (authorization, authFailResponse)
import Types.Domain.Environment (Environment(..))
import qualified Types.API.News as API 
import qualified Types.Database.User as Database
import Network.HTTP.Types (status400, status404, hContentType)
import Network.Wai (Request, Response, responseLBS, getRequestBodyChunk)
import Data.Aeson (decodeStrict, FromJSON)
import Control.Monad.Reader (asks, ReaderT, lift)

withParsedRequest :: FromJSON a => (a -> ReaderT Environment IO Response) -> Request -> ReaderT Environment IO Response
withParsedRequest f req = do
  rawJSON <- lift $ getRequestBodyChunk req
  let decodedReq = decodeStrict rawJSON
  case decodedReq of 
    Nothing -> do
      addLog WARNING "Invalid JSON"
      pure $ responseLBS status400 [(hContentType, "text/plain")] "Bad Request: Invalid JSON\n"
    Just parsedReq -> f parsedReq

withAuthAndParsedRequest :: FromJSON a 
                         => (Database.User -> a -> ReaderT Environment IO Response) 
                         -> Request 
                         -> ReaderT Environment IO Response
withAuthAndParsedRequest f req = do
  conn <- asks dbConnection
  eiInvoker <- lift $ authorization conn req 
  case eiInvoker of
    Left err -> do
      addLog WARNING "Authorization fail"
      pure $ responseLBS status404 [(hContentType, "text/plain")] "404 : Not Found"
    Right invoker -> do
      rawJSON <- lift $ getRequestBodyChunk req
      case decodeStrict rawJSON of 
        Nothing -> do
          addLog WARNING "Invalid JSON"
          pure $ responseLBS status400 [(hContentType, "text/plain")] "Bad Request: Invalid JSON"
        Just parsedReq -> f invoker parsedReq

withAuth :: (Database.User -> Bool)
      -> (Request -> ReaderT Environment IO Response) 
      -> Request 
      -> ReaderT Environment IO Response
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