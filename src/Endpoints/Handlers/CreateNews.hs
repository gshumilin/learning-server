{-# LANGUAGE MultiWayIf #-}

module Endpoints.Handlers.CreateNews where

import Data.Maybe (isJust, isNothing)
import qualified Data.Text as T
import qualified Types.API.News as API (CreateNewsRequest (..))
import qualified Types.DB.Category as DB (Category (..))
import qualified Types.DB.User as DB (User (..))
import Types.Domain.Picture (Picture (..))

data CreateNewsResult = NotAbleToCreateNews | IncorrectTitle | InvalidPictureFormat | CategoryNotExists | CreateNewsSuccess Int deriving (Show, Eq)

data Handle m = Handle
  { hReadCategoryById :: Int -> m (Maybe DB.Category),
    hFindNewsIdByTitle :: T.Text -> m (Maybe Int),
    hWriteNews :: Int -> API.CreateNewsRequest -> m Int
  }

hCreateNews :: Monad m => Handle m -> DB.User -> API.CreateNewsRequest -> m CreateNewsResult
hCreateNews Handle {..} DB.User {..} req = do
  isTitleInvalidResult <- isTitleInvalid req
  let isPicsFormatInvalidRes = isPicsFormatInvalid req
  isCategoryInvalidResult <- isCategoryInvalid req
  if
      | not isAbleToCreateNews -> pure NotAbleToCreateNews
      | isTitleInvalidResult -> pure IncorrectTitle
      | isPicsFormatInvalidRes -> pure InvalidPictureFormat
      | isCategoryInvalidResult -> pure CategoryNotExists
      | otherwise -> do
        resId <- hWriteNews userId req
        pure $ CreateNewsSuccess resId
  where
    isTitleInvalid API.CreateNewsRequest {..} = do
      res <- hFindNewsIdByTitle title
      pure $ isJust res

    isPicsFormatInvalid API.CreateNewsRequest {..} =
      case pictures of
        Nothing -> False
        Just picList ->
          let validList = ["image/png", "image/jpeg", "image/bmp"]
           in not $ all (\Picture {..} -> T.toLower mime `elem` validList) picList

    isCategoryInvalid API.CreateNewsRequest {..} = do
      mbSomeCat <- hReadCategoryById categoryId
      pure $ isNothing mbSomeCat
