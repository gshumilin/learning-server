module Endpoints.Categories where

import Types.Domain.Category
import qualified Types.Database.Category as DBType
import Types.Domain.Environment
import Network.HTTP.Types (hContentType, status200, status400)
import Network.Wai
import Data.Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.List (find)
import Data.Maybe (fromMaybe)
import DataBaseQueries.Category (parseCategoriesList, parseSpecificCategory)
import Control.Monad (mapM)
import Control.Monad.Reader

dbCategoryTransform :: Integer -> ReaderT Environment IO Category
dbCategoryTransform dbCatId = do
    conn <- asks dbConnection
    dbCategoriesList <- lift $ parseCategoriesList conn
    let mbDbCat = find (\c -> DBType.categoryID c == dbCatId) dbCategoriesList
    case mbDbCat of
        Nothing -> error "dbCategoryTransform didn't find dbCategory"
        Just DBType.Category {..} -> do
            case parentID of 
                Nothing -> return $ Category categoryID title Nothing
                Just parId -> do
                    let mbDbParentCat = find (\c -> categoryID == 1) dbCategoriesList
                    case mbDbParentCat of
                        Nothing -> return $ Category categoryID title Nothing
                        Just dbParentCat -> do
                            parentCat <- dbCategoryTransform (DBType.categoryID dbParentCat)
                            return $ Category categoryID title (Just parentCat)

getCategoriesList :: ReaderT Environment IO Response
getCategoriesList = do
    conn <- asks dbConnection
    dbCatList <- lift $ parseCategoriesList conn -- :: [DBType.Categoy]
    catList <- mapM dbCategoryTransform (map (DBType.categoryID) dbCatList)
    let jsonNewsList = encodePretty catList
    return $ responseLBS status200 [(hContentType, "text/plain")] $ jsonNewsList

fromDbCategoryList :: [DBType.Category] -> Category
fromDbCategoryList (x:xs) = 
    case DBType.parentID x of
        Nothing -> Category (DBType.categoryID x) (DBType.title x) Nothing
        Just id -> Category (DBType.categoryID x) (DBType.title x) parentCategory
            where
                parentCategory = if (null xs) then Nothing else Just $ fromDbCategoryList xs

getSpecificCategory :: Integer -> ReaderT Environment IO Category
getSpecificCategory id = do
    conn <- asks dbConnection
    categoryWithParrents <- lift $ parseSpecificCategory id conn
    let domainCategory = fromDbCategoryList categoryWithParrents
    return domainCategory
    
createCategory = undefined