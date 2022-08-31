module Endpoints.Categories where

import qualified Types.Domain.Category as Domain
import qualified Types.Database.Category as DBType
import qualified Types.API.Category as API
import Types.Domain.Environment
import Database.PostgreSQL.Simple
import Network.HTTP.Types (hContentType, status200, status400)
import Network.Wai
import Data.Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.List (find)
import Data.Maybe (fromMaybe)
import DatabaseQueries.Category (parseCategoriesList, readCategoryWithParentsById, writeCategory, rewriteCategory)
import Control.Monad (mapM)
import Control.Monad.Reader

dbCategoryTransform :: Int -> ReaderT Environment IO Domain.Category
dbCategoryTransform dbCatId = do
  conn <- asks dbConnection
  dbCategoriesList <- lift $ parseCategoriesList conn
  let mbDbCat = find (\c -> DBType.categoryID c == dbCatId) dbCategoriesList
  case mbDbCat of
    Nothing -> error "dbCategoryTransform didn't find dbCategory"
    Just DBType.Category {..} -> do
      case parentID of 
        Nothing -> pure $ Domain.Category categoryID title Nothing
        Just parId -> do
          let mbDbParentCat = find (\c -> categoryID == 1) dbCategoriesList
          case mbDbParentCat of
            Nothing -> pure $ Domain.Category categoryID title Nothing
            Just dbParentCat -> do
              parentCat <- dbCategoryTransform (DBType.categoryID dbParentCat)
              pure $ Domain.Category categoryID title (Just parentCat)

getCategoriesList :: ReaderT Environment IO Response
getCategoriesList = do
  conn <- asks dbConnection
  dbCatList <- lift $ parseCategoriesList conn -- :: [DBType.Categoy]
  catList <- mapM (dbCategoryTransform . DBType.categoryID) dbCatList
  let jsonNewsList = encodePretty catList
  pure $ responseLBS status200 [(hContentType, "text/plain")] jsonNewsList

fromDbCategoryList :: [DBType.Category] -> Domain.Category
fromDbCategoryList (x:xs) = 
  case DBType.parentID x of
    Nothing -> Domain.Category (DBType.categoryID x) (DBType.title x) Nothing
    Just id -> Domain.Category (DBType.categoryID x) (DBType.title x) parentCategory
      where
        parentCategory = if null xs then Nothing else Just $ fromDbCategoryList xs

getSpecificCategory :: Connection -> Int -> IO Domain.Category
getSpecificCategory conn id = do
  categoryWithParrents <- readCategoryWithParentsById id conn
  let domainCategory = fromDbCategoryList categoryWithParrents
  pure domainCategory

createCategory :: Request -> ReaderT Environment IO Response
createCategory request = do
  conn <- asks dbConnection
  rawJSON <- lift $ getRequestBodyChunk request
  let decodedReq = decodeStrict rawJSON :: Maybe API.CreateCategoryRequest
  case decodedReq of
    Nothing -> do 
      lift $ putStrLn "Invalid JSON" -- log
      pure $ responseLBS status400 [(hContentType, "text/plain")] "Bad Request: Invalid JSON\n"
    Just newCategory -> do
      lift $ writeCategory conn newCategory
      pure $ responseLBS status200 [(hContentType, "text/plain")] "all done"

editCategory :: Request -> ReaderT Environment IO Response
editCategory request = do
  conn <- asks dbConnection
  rawJSON <- lift $ getRequestBodyChunk request
  let decodedReq = decodeStrict rawJSON :: Maybe API.EditCategoryRequest
  case decodedReq of 
    Nothing -> do
      lift $ putStrLn "Invalid JSON"
      pure $ responseLBS status400 [(hContentType, "text/plain")] "Bad Request: Invalid JSON\n"
    Just editedCategory -> do
      lift $ rewriteCategory conn editedCategory
      pure $ responseLBS status200 [(hContentType, "text/plain")] "all done"
