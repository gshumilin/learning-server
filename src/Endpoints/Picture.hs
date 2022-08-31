module Endpoints.Picture where

import Control.Monad.Reader
import Data.Aeson (decodeStrict)
import Data.ByteString.Base64 (decodeBase64)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Internal as LBS
import Data.List (find)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T (encodeUtf8)
import DatabaseQueries.Picture (readPicture, writePicture)
import Log (addLog)
import Network.HTTP.Types (hContentType, status200, status400, status404)
import Network.Wai
import Text.Read (readMaybe)
import Types.Domain.Environment
import Types.Domain.Log
import Types.Domain.Picture

--подразумевается, что все картинки передаются в формате png
--параметр запроса картинки - айдишник картинки
--при запросе новостей клиент получает не картинку, а ссылку на неё в формате "localhost:3000/picture"
--в ответ на запрос клиент получит либо текст с ошибкой "No such picture", либо раскодированную картинку.

getPicture :: Request -> ReaderT Environment IO Response
getPicture req = do
  case findPicId req of
    Left err -> pure . responseLBS status400 [(hContentType, "text/plain")] $ err
    Right picId -> do
      conn <- asks dbConnection
      mbPic <- lift $ readPicture conn picId
      case mbPic of
        Nothing -> pure $ responseLBS status404 [(hContentType, "text/plain")] "Not Found 404"
        Just pic@Picture {..} -> do
          case decodeBase64 . BS.pack . T.unpack $ picData of
            Left err -> do
              addLog WARNING $ "-----data decoding error: " ++ show err
              pure $ responseLBS status404 [(hContentType, "text/plain")] "Not Found 404"
            Right decodedPic -> pure $ responseLBS status200 [(hContentType, T.encodeUtf8 mime)] . LBS.fromStrict $ decodedPic

findPicId :: Request -> Either LBS.ByteString Int
findPicId req =
  case find (\(k, v) -> k == "id") $ queryString req of
    Nothing -> Left "'id' parameter not specified"
    Just (_, Nothing) -> Left "empty value of the 'id' parameter"
    Just (_, Just bsPicID) -> do
      case (readMaybe $ BS.unpack bsPicID :: Maybe Int) of
        Nothing -> do
          Left "invalid 'id' parameter value"
        Just picID -> Right picID

putPicture :: Request -> ReaderT Environment IO Response
putPicture request = do
  conn <- asks dbConnection
  rawJSON <- lift $ getRequestBodyChunk request
  let decodedReq = decodeStrict rawJSON :: Maybe Picture
  case decodedReq of
    Nothing -> do
      lift $ putStrLn "Invalid JSON" -- log
      pure $ responseLBS status400 [(hContentType, "text/plain")] "Bad Request: Invalid JSON\n"
    Just newBase64 -> do
      lift $ writePicture conn newBase64
      pure $ responseLBS status200 [(hContentType, "text/plain")] "all done"
