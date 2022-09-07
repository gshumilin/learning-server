module Endpoints.Handlers.CreateNews where

import Data.Maybe (isNothing)
import qualified Types.API.News as API
import qualified Types.DB.Category as DB (Category (..))
import qualified Types.DB.User as DB (User (..))

data CreateNewsResult = NotAbleToCreateNews | CategoryNotExists | CreateNewsSuccess deriving (Show, Eq)

data Handle m = Handle
  { hReadCategoryById :: Int -> m (Maybe DB.Category),
    hWriteNews :: Int -> API.CreateNewsRequest -> m ()
  }

hCreateNews :: Monad m => Handle m -> DB.User -> API.CreateNewsRequest -> m CreateNewsResult
hCreateNews Handle {..} invoker req = do
  if not (DB.isAbleToCreateNews invoker)
    then pure NotAbleToCreateNews
    else do
      mbSomeCat <- hReadCategoryById $ API.categoryID req
      if isNothing mbSomeCat
        then pure CategoryNotExists
        else do
          hWriteNews (DB.userID invoker) req
          pure CreateNewsSuccess
