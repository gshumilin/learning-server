{-# LANGUAGE MultiWayIf #-}

module Endpoints.Handlers.EditCategory where

import qualified Data.Text as T
import Data.Maybe (isJust)
import qualified Types.Database.Category as DB
import qualified Types.Database.User as DB
import qualified Types.API.Category as API

data EditCategoryResult = NotAdmin | CategoryNotExists | IncorrectParentId | IncorrectTitle | EditCategorySuccess deriving (Show, Eq)

data Handle m = Handle
  { hReadCategoryById :: Int -> m (Maybe DB.Category),
    hReadCategoryByTitle :: T.Text -> m (Maybe DB.Category),
    hRewriteCategory :: API.EditCategoryRequest -> m ()
  }

hEditCategory :: Monad m => Handle m -> DB.User -> API.EditCategoryRequest -> m EditCategoryResult
hEditCategory Handle {..} DB.User{..} req@API.EditCategoryRequest {..} = do
  isNotExist <- isNotExistCheck processedCategoryID
  isTitleBad <- isNewTitleBadCheck newTitle
  isParentBad <- isParentBadCheck newParentCategoryID processedCategoryID
  if | not isAdmin -> return NotAdmin
     | isNotExist -> return CategoryNotExists
     | isTitleBad -> return IncorrectTitle
     | isParentBad -> return IncorrectParentId
     | otherwise -> do 
        hRewriteCategory req
        return EditCategorySuccess
  where
    isNotExistCheck currId = do
      mbCurrCat <- hReadCategoryById currId
      return . not . isJust $ mbCurrCat
    
    isNewTitleBadCheck Nothing = return False
    isNewTitleBadCheck (Just t) = do
      mbSomeCat <- hReadCategoryByTitle t
      return (isJust mbSomeCat)
    
    isParentBadCheck Nothing _ = return False
    isParentBadCheck (Just 0) curId = return False
    isParentBadCheck (Just parId) curId = do
      if parId == curId
        then return True
        else do
          mbParCat <- hReadCategoryById parId
          case mbParCat of
            Nothing -> return True
            Just parCat -> pure $ DB.parentID parCat == Just curId