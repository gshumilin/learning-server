{-# LANGUAGE MultiWayIf #-}

module Endpoints.Handlers.EditCategory where

import Control.Monad.Reader (ReaderT)
import Data.Maybe (isJust, isNothing)
import qualified Data.Text as T
import qualified Types.API.Category as API (EditCategoryRequest (..))
import qualified Types.DB.Category as DB (Category (..))
import qualified Types.DB.User as DB (User (..))
import Types.Domain.Environment (Environment (..))

data EditCategoryResult = NotAdmin | CategoryNotExists | IncorrectParentId | IncorrectTitle | EditCategorySuccess deriving (Show, Eq)

data Handle m = Handle
  { hReadCategoryById :: Int -> ReaderT Environment m (Maybe DB.Category),
    hReadCategoryByTitle :: T.Text -> ReaderT Environment m (Maybe DB.Category),
    hRewriteCategory :: API.EditCategoryRequest -> ReaderT Environment m ()
  }

hEditCategory :: Monad m => Handle m -> DB.User -> API.EditCategoryRequest -> ReaderT Environment m EditCategoryResult
hEditCategory Handle {..} DB.User {..} req@API.EditCategoryRequest {..} = do
  isNotExist <- isNotExistCheck processedCategoryId
  isTitleBad <- isNewTitleBadCheck newTitle
  isParentBad <- isParentBadCheck newParentCategoryId processedCategoryId
  if
      | not isAdmin -> pure NotAdmin
      | isNotExist -> pure CategoryNotExists
      | isTitleBad -> pure IncorrectTitle
      | isParentBad -> pure IncorrectParentId
      | otherwise -> do
        hRewriteCategory req
        pure EditCategorySuccess
  where
    isNotExistCheck currId = do
      mbCurrCat <- hReadCategoryById currId
      pure . isNothing $ mbCurrCat

    isNewTitleBadCheck Nothing = pure False
    isNewTitleBadCheck (Just t) = do
      mbSomeCat <- hReadCategoryByTitle t
      pure (isJust mbSomeCat)

    isParentBadCheck Nothing _ = pure False
    isParentBadCheck (Just 0) _ = pure False
    isParentBadCheck (Just parId) curId = do
      if parId == curId
        then pure True
        else do
          mbParCat <- hReadCategoryById parId
          case mbParCat of
            Nothing -> pure True
            Just parCat -> pure $ DB.parentId parCat == Just curId
