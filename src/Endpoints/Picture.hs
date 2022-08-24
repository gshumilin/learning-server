module Endpoints.Picture where

import Types.Domain.Environment
import Types.Domain.Picture
import Control.Monad.Reader
import Network.Wai
import DatabaseQueries.Picture (readPicture, writePicture)
import Network.HTTP.Types (hContentType, status200, status400, status404)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Internal as LBS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T 
import qualified Data.Text.Encoding as T (encodeUtf8)
import Data.ByteString.Base64 (decodeBase64)
import Text.Read (readMaybe)
import Data.List (find)
import Data.Aeson (decodeStrict)

--подразумевается, что все картинки передаются в формате png
--параметр запроса картинки - айдишник картинки
--при запросе новостей клиент получает не картинку, а ссылку на неё в формате "localhost:3000/picture"
--в ответ на запрос клиент получит либо текст с ошибкой "No such picture", либо раскодированную картинку.

getPicture :: Request -> ReaderT Environment IO (Response)
getPicture req = do
    case findPicId req of
        Left err -> return . responseLBS status400 [(hContentType, "text/plain")] $ err
        Right picId -> do
            conn <- asks dbConnection
            mbPic <- lift $ readPicture conn picId
            case mbPic of
                Nothing -> return $ responseLBS status404 [(hContentType, "text/plain")] $ "Not Found 404"
                Just pic@Picture {..} -> do
                    case decodeBase64 . BS.pack . T.unpack $ picData of
                        Left err -> do
                            lift . putStrLn $ "-----data decoding error: " ++ show err --log
                            return $ responseLBS status404 [(hContentType, "text/plain")] $ "Not Found 404"
                        Right decodedPic -> return $ responseLBS status200 [(hContentType, (T.encodeUtf8 mime))] . LBS.fromStrict $ decodedPic

findPicId :: Request -> Either LBS.ByteString Int
findPicId req = 
    case find (\(k,v) -> k == "id") $ queryString req of
        Nothing -> Left $ "'id' parameter not specified"
        Just (_, Nothing) -> Left $ "empty value of the 'id' parameter"
        Just (_, Just bsPicID) -> do
            case (readMaybe $ BS.unpack bsPicID :: Maybe Int) of
                Nothing -> do
                    Left $ "invalid 'id' parameter value"
                Just picID -> Right picID     

putPicture :: Request -> ReaderT Environment IO Response
putPicture request = do
    conn <- asks dbConnection
    rawJSON <- lift $ getRequestBodyChunk request
    let decodedReq = decodeStrict rawJSON :: Maybe Picture
    case decodedReq of
        Nothing -> do 
            lift $ putStrLn "Invalid JSON" -- log
            return $ responseLBS status400 [(hContentType, "text/plain")] $ "Bad Request: Invalid JSON\n"
        Just newBase64 -> do
            lift . putStrLn . show $ rawJSON
            lift $ writePicture conn newBase64
            return $ responseLBS status200 [(hContentType, "text/plain")] $ "all done"