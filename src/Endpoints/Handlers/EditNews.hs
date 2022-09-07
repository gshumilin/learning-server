module Endpoints.Handlers.EditNews where

import qualified Types.API.News as API (EditNewsRequest (..))
import qualified Types.DB.News as DB (EditedNewsFields (..))
import qualified Types.DB.User as DB (User (..))

data EditNewsResult = NotAuthor | NewsNotExists | EditNewsSuccess deriving (Show, Eq)

data Handle m = Handle
  { hReadSpecificNews :: Int -> m (Maybe DB.EditedNewsFields),
    hRewriteNews :: DB.EditedNewsFields -> API.EditNewsRequest -> m ()
  }

hEditNews :: Monad m => Handle m -> DB.User -> API.EditNewsRequest -> m EditNewsResult
hEditNews Handle {..} invoker editNewsRequest@API.EditNewsRequest {..} = do
  mbCurrentNews <- hReadSpecificNews newsID
  case mbCurrentNews of
    Nothing -> pure NewsNotExists
    Just editedNewsFields@DB.EditedNewsFields {..} -> do
      if DB.userID invoker == oldCreatorID
        then do
          hRewriteNews editedNewsFields editNewsRequest
          pure EditNewsSuccess
        else pure NotAuthor
