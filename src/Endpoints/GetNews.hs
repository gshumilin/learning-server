module Endpoints.GetNews where

import Control.Monad.Reader (ReaderT)
import Data.Aeson.Encode.Pretty (encodePretty)
import DatabaseQueries.News (readNews)
import Network.HTTP.Types (hContentType, status200)
import Network.Wai (Request, Response, responseLBS)
import Types.Domain.Environment (Environment (..))
import qualified Types.Domain.News as Domain (NewsList (..))

getNews :: Request -> ReaderT Environment IO Response
getNews req = do
  newsList <- readNews req
  let jsonNewsList = encodePretty $ Domain.NewsList newsList
  pure $ responseLBS status200 [(hContentType, "text/plain")] jsonNewsList
