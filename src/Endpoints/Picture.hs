module Endpoints.Picture where

import Types.Domain.Environment
import Types.Domain.Picture
import Control.Monad.Reader
import Network.Wai
import DataBaseQueries.Picture (findPicture)
import Network.HTTP.Types (hContentType, status200, status400)
import Data.ByteString.Lazy.Internal (packChars)

getPicture :: Int -> ReaderT Environment IO (Response)
getPicture id = do
    conn <- asks dbConnection
    res <- lift $ findPicture conn id
    case res of
        Nothing -> return $ responseLBS status200 [(hContentType, "text/plain")] $ "No such picture"
        Just pic -> return $ responseLBS status200 [(hContentType, "text/plain")] $ (packChars . show . base64 $ pic)