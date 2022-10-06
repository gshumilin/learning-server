module Endpoints.Handlers.EditNews where

import Control.Monad.Reader (ReaderT)
import qualified Types.API.News as API (EditNewsRequest (..))
import qualified Types.DB.News as DB (EditedNewsFields (..))
import Types.Domain.Environment (Environment (..))

data EditNewsResult = NewsNotExistsForThisAuthor | EditNewsSuccess deriving (Show, Eq)

data Handle m = Handle
  { hReadSpecificNews :: Int -> ReaderT Environment m (Maybe DB.EditedNewsFields),
    hRewriteNews :: DB.EditedNewsFields -> API.EditNewsRequest -> ReaderT Environment m ()
  }

hEditNews :: Monad m => Handle m -> API.EditNewsRequest -> ReaderT Environment m EditNewsResult
hEditNews Handle {..} editNewsRequest@API.EditNewsRequest {..} = do
  mbCurrentNews <- hReadSpecificNews newsId
  case mbCurrentNews of
    Nothing -> pure NewsNotExistsForThisAuthor
    Just editedNewsFields -> do
      hRewriteNews editedNewsFields editNewsRequest
      pure EditNewsSuccess
