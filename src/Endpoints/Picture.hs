module Endpoints.Picture where

import Control.Monad.Reader (ReaderT, lift)
import Data.ByteString.Base64 (decodeBase64)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import Data.List (find)
import qualified Data.Text as T (pack, unpack)
import qualified Data.Text.Encoding as T (encodeUtf8)
import DatabaseQueries.Picture (readPicture)
import Log (addLog)
import Network.HTTP.Types (hContentType, status200, status400, status404)
import Network.Wai (Request, Response, queryString, responseLBS)
import Text.Read (readMaybe)
import Types.Domain.Environment (Environment (..))
import Types.Domain.Log (LogLvl (..))
import qualified Types.Domain.Picture as Domain (Picture (..))
import Utils (askConnection)

getPicture :: Request -> ReaderT Environment IO Response
getPicture req = do
  case findPicId req of
    Left err -> pure . responseLBS status400 [(hContentType, "text/plain")] $ err
    Right picId -> do
      conn <- askConnection
      mbPic <- lift $ readPicture conn picId
      case mbPic of
        Nothing -> pure $ responseLBS status404 [(hContentType, "text/plain")] "Not Found 404"
        Just Domain.Picture {..} -> do
          case decodeBase64 . BS.pack . T.unpack $ picData of
            Left err -> do
              addLog WARNING $ "-----data decoding error: " <> T.pack (show err)
              pure $ responseLBS status404 [(hContentType, "text/plain")] "Not Found 404"
            Right decodedPic -> pure $ responseLBS status200 [(hContentType, T.encodeUtf8 mime)] . LBS.fromStrict $ decodedPic

findPicId :: Request -> Either LBS.ByteString Int
findPicId req =
  case find (\(k, _) -> k == "id") $ queryString req of
    Nothing -> Left "'id' parameter not specified"
    Just (_, Nothing) -> Left "empty value of the 'id' parameter"
    Just (_, Just bsPicId) -> do
      case (readMaybe $ BS.unpack bsPicId :: Maybe Int) of
        Nothing -> do
          Left "invalid 'id' parameter value"
        Just picId -> Right picId
