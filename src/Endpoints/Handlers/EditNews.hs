module Endpoints.Handlers.EditNews where

import qualified Types.Database.News as DBType
import qualified Types.Database.User as DBType
import qualified Types.API.News as API

data EditNewsResult = NotAuthor | NewsNotExists | EditNewsSuccess deriving (Show, Eq)

data Handle m = Handle
  { hReadSpecificNews :: Int -> m (Maybe DBType.EditedNewsFields),
    hRewriteNews :: DBType.EditedNewsFields -> API.EditNewsRequest -> m ()
  }

hEditNews :: Monad m => Handle m -> DBType.User -> API.EditNewsRequest -> m EditNewsResult
hEditNews Handle {..} invoker editNewsRequest@API.EditNewsRequest {..} = do
  mbCurrentNews <- hReadSpecificNews newsID
  case mbCurrentNews of
    Nothing -> return NewsNotExists
    Just editedNewsFields@DBType.EditedNewsFields {..} -> do
      if DBType.userID invoker == oldCreatorID
        then do
          hRewriteNews editedNewsFields editNewsRequest
          return EditNewsSuccess
        else return NotAuthor