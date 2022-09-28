module Endpoints.Handlers.CreateNews where

import Data.Maybe (isNothing)
import qualified Data.Text as T
import qualified Types.API.News as API (CreateNewsRequest (..))
import qualified Types.DB.Category as DB (Category (..))
import qualified Types.DB.User as DB (User (..))
import Types.Domain.Picture (Picture (..))

data CreateNewsResult = NotAbleToCreateNews | InvalidPictureFormat | CategoryNotExists | CreateNewsSuccess Int deriving (Show, Eq)

data Handle m = Handle
  { hReadCategoryById :: Int -> m (Maybe DB.Category),
    hWriteNews :: Int -> API.CreateNewsRequest -> m Int
  }

hCreateNews :: Monad m => Handle m -> DB.User -> API.CreateNewsRequest -> m CreateNewsResult
hCreateNews Handle {..} invoker req = do
  if not (DB.isAbleToCreateNews invoker)
    then pure NotAbleToCreateNews
    else
      if not . isPicsFormatCorrect $ req
        then pure InvalidPictureFormat
        else do
          mbSomeCat <- hReadCategoryById $ API.categoryId req
          if isNothing mbSomeCat
            then pure CategoryNotExists
            else do
              resId <- hWriteNews (DB.userId invoker) req
              pure $ CreateNewsSuccess resId

isPicsFormatCorrect :: API.CreateNewsRequest -> Bool
isPicsFormatCorrect API.CreateNewsRequest {..} =
  case pictures of
    Nothing -> True
    Just picList ->
      let validList = ["image/png", "image/jpeg", "image/bmp"]
       in all (\Picture {..} -> T.toLower mime `elem` validList) picList
