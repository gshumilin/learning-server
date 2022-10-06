module Utils where

import Auth (authorization)
import Control.Monad.Reader (ReaderT, asks, lift)
import Data.Aeson (FromJSON, decodeStrict)
import Data.ByteString.Char8 (pack)
import Data.ByteString.Lazy (ByteString, fromStrict)
import Data.Pool (takeResource, withResource)
import Database.PostgreSQL.Simple (Connection)
import Log (addLog)
import Network.HTTP.Types (hContentType, status400, status404)
import Network.Wai (Request, Response, getRequestBodyChunk, responseLBS)
import qualified Types.DB.User as DB (User (..))
import Types.Domain.Environment (Environment (..))
import Types.Domain.Log (LogLvl (..))

withAuthAndParsedRequest ::
  FromJSON a =>
  (DB.User -> a -> ReaderT Environment IO Response) ->
  Request ->
  ReaderT Environment IO Response
withAuthAndParsedRequest f req = do
  conn <- askConnection
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

intToLBS :: Int -> ByteString
intToLBS = fromStrict . pack . show

askConnection :: ReaderT Environment IO Connection
askConnection = do
  pool <- asks dbPool
  (conn, _) <- lift $ takeResource pool
  pure conn

withPool :: (Connection -> IO a) -> ReaderT Environment IO a
withPool f = do
  pool <- asks dbPool
  lift $ withResource pool f
