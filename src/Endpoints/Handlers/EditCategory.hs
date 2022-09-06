{-# LANGUAGE MultiWayIf #-}

module Endpoints.Handlers.EditCategory where

import Data.Maybe (isJust, isNothing)
import qualified Data.Text as T
import qualified Types.API.Category as API
import qualified Types.DB.Category as DB
import qualified Types.DB.User as DB

data EditCategoryResult = NotAdmin | CategoryNotExists | IncorrectParentId | IncorrectTitle | EditCategorySuccess deriving (Show, Eq)

data Handle m = Handle
  { hReadCategoryById :: Int -> m (Maybe DB.Category),
    hReadCategoryByTitle :: T.Text -> m (Maybe DB.Category),
    hRewriteCategory :: API.EditCategoryRequest -> m ()
  }

hEditCategory :: Monad m => Handle m -> DB.User -> API.EditCategoryRequest -> m EditCategoryResult
hEditCategory Handle {..} DB.User {..} req@API.EditCategoryRequest {..} = do
  isNotExist <- isNotExistCheck processedCategoryID
  isTitleBad <- isNewTitleBadCheck newTitle
  isParentBad <- isParentBadCheck newParentCategoryID processedCategoryID
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
            Just parCat -> pure $ DB.parentID parCat == Just curId
