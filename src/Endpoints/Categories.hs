module Endpoints.Categories where

import Types.Category
import Types.Environment
import Network.HTTP.Types (hContentType, status200, status400)
import Network.Wai
import Data.Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import DataBaseQueries (parseCategoriesList)
import Control.Monad.Reader

getCategoriesList :: ReaderT Environment IO (Response)
getCategoriesList = do
    conn <- asks dbConnection
    catList <- lift $ DataBaseQueries.parseCategoriesList conn
    let jsonNewsList = encodePretty catList
    return $ responseLBS status200 [(hContentType, "text/plain")] $ jsonNewsList

createCategory = undefined