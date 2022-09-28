module Endpoints.Handlers.CreateNews where

import Data.Maybe (isNothing)
import qualified Types.API.News as API (CreateNewsRequest (..))
import qualified Types.DB.Category as DB (Category (..))
import qualified Types.DB.User as DB (User (..))

data CreateNewsResult = NotAbleToCreateNews | CategoryNotExists | CreateNewsSuccess Int deriving (Show, Eq)

data Handle m = Handle
  { hReadCategoryById :: Int -> m (Maybe DB.Category),
    hWriteNews :: Int -> API.CreateNewsRequest -> m Int
  }

hCreateNews :: Monad m => Handle m -> DB.User -> API.CreateNewsRequest -> m CreateNewsResult
hCreateNews Handle {..} invoker req = do
  if not (DB.isAbleToCreateNews invoker)
    then pure NotAbleToCreateNews
    else do
      mbSomeCat <- hReadCategoryById $ API.categoryId req
      if isNothing mbSomeCat
        then pure CategoryNotExists
        else do
          resId <- hWriteNews (DB.userId invoker) req
          pure $ CreateNewsSuccess resId
