{-# LANGUAGE MultiWayIf #-}

module Endpoints.Handlers.CreateCategory where

import Data.Maybe (isJust, isNothing)
import qualified Data.Text as T
import qualified Types.API.Category as API
import qualified Types.DB.Category as DB
import qualified Types.DB.User as DB

data CreateCategoryResult = NotAdmin | IncorrectParentId | IncorrectTitle | CreateCategorySuccess deriving (Show, Eq)

data Handle m = Handle
  { hReadCategoryById :: Int -> m (Maybe DB.Category),
    hReadCategoryByTitle :: T.Text -> m (Maybe DB.Category),
    hWriteCategory :: API.CreateCategoryRequest -> m ()
  }

hCreateCategory :: Monad m => Handle m -> DB.User -> API.CreateCategoryRequest -> m CreateCategoryResult
hCreateCategory Handle {..} DB.User {..} req@API.CreateCategoryRequest {..} = do
  isParentBad <- isParentBadCheck parentCategoryID
  isTitleBad <- isTitleBadCheck title
  if
      | not isAdmin -> pure NotAdmin
      | isParentBad -> pure IncorrectParentId
      | isTitleBad -> pure IncorrectTitle
      | otherwise -> do
        hWriteCategory req
        pure CreateCategorySuccess
  where
    isParentBadCheck mbParId =
      case mbParId of
        Nothing -> pure False
        Just parId -> do
          mbParCat <- hReadCategoryById parId
          pure $ isNothing mbParCat

    isTitleBadCheck t = do
      someCat <- hReadCategoryByTitle t
      pure $ isJust someCat

-- data CreateCategoryRequest = CreateCategoryRequest
--   { title :: T.Text,
--     parentCategoryID :: Maybe Int
--   }
