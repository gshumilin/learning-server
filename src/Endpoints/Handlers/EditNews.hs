module Endpoints.Handlers.EditNews where

import qualified Types.API.News as API (EditNewsRequest (..))
import qualified Types.DB.News as DB (EditedNewsFields (..))

data EditNewsResult = NewsNotExistsForThisAuthor | EditNewsSuccess deriving (Show, Eq)

data Handle m = Handle
  { hReadSpecificNews :: Int -> m (Maybe DB.EditedNewsFields),
    hRewriteNews :: DB.EditedNewsFields -> API.EditNewsRequest -> m ()
  }

hEditNews :: Monad m => Handle m -> API.EditNewsRequest -> m EditNewsResult
hEditNews Handle {..} editNewsRequest@API.EditNewsRequest {..} = do
  mbCurrentNews <- hReadSpecificNews newsId
  case mbCurrentNews of
    Nothing -> pure NewsNotExistsForThisAuthor
    Just editedNewsFields -> do
      hRewriteNews editedNewsFields editNewsRequest
      pure EditNewsSuccess
