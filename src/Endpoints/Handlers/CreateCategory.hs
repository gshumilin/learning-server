{-# LANGUAGE MultiWayIf #-}

module Endpoints.Handlers.CreateCategory where

import Control.Monad.Reader (ReaderT)
import Data.Maybe (isJust, isNothing)
import qualified Data.Text as T
import qualified Types.API.Category as API (CreateCategoryRequest (..))
import qualified Types.DB.Category as DB (Category (..))
import qualified Types.DB.User as DB (User (..))
import Types.Domain.Environment (Environment (..))

data CreateCategoryResult = NotAdmin | IncorrectParentId | IncorrectTitle | CreateCategorySuccess Int deriving (Show, Eq)

data Handle m = Handle
  { hReadCategoryById :: Int -> ReaderT Environment m (Maybe DB.Category),
    hReadCategoryByTitle :: T.Text -> ReaderT Environment m (Maybe DB.Category),
    hWriteCategory :: API.CreateCategoryRequest -> ReaderT Environment m Int
  }

hCreateCategory :: Monad m => Handle m -> DB.User -> API.CreateCategoryRequest -> ReaderT Environment m CreateCategoryResult
hCreateCategory Handle {..} DB.User {..} req@API.CreateCategoryRequest {..} = do
  isParentBad <- isParentBadCheck parentCategoryId
  isTitleBad <- isTitleBadCheck title
  if
      | not isAdmin -> pure NotAdmin
      | isParentBad -> pure IncorrectParentId
      | isTitleBad -> pure IncorrectTitle
      | otherwise -> do
        resId <- hWriteCategory req
        pure $ CreateCategorySuccess resId
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
