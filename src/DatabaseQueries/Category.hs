module DatabaseQueries.Category where

import qualified Data.Text as T
import Database.PostgreSQL.Simple (Connection, Only (..), execute, query, query_)
import qualified Types.API.Category as API (CreateCategoryRequest (..), EditCategoryRequest (..))
import qualified Types.DB.Category as DB (Category (..))

parseCategoriesList :: Connection -> IO [DB.Category]
parseCategoriesList conn = query_ conn "SELECT * FROM categories"

readCategoryById :: Connection -> Int -> IO (Maybe DB.Category)
readCategoryById conn catID = do
  let q = "SELECT * FROM categories WHERE id=?"
  res <- query conn q (Only catID)
  case res of
    [] -> pure Nothing
    (c : _) -> pure $ Just c

readCategoryByTitle :: Connection -> T.Text -> IO (Maybe DB.Category)
readCategoryByTitle conn title = do
  let q = "SELECT * FROM categories WHERE title=?"
  res <- query conn q (Only title)
  case res of
    [] -> pure Nothing
    (c : _) -> pure $ Just c

readCategoryWithParentsById :: Int -> Connection -> IO [DB.Category]
readCategoryWithParentsById catId conn = do
  let q = "with recursive records_list as (select n.id as id, n.title as title, n.parent_category_id as parent_category_id, 1 as depth from categories as n where id = ? union all select n.id as id, n.title as title, n.parent_category_id as parent_category_id, records_list.depth + 1 as depth from categories as n  join records_list on records_list.parent_category_id = n.id ) select records_list.id, records_list.title, records_list.parent_category_id from records_list order by depth;"
  query conn q (Only catId)

writeCategory :: Connection -> API.CreateCategoryRequest -> IO ()
writeCategory conn API.CreateCategoryRequest {..} = do
  _ <-
    execute
      conn
      "INSERT INTO categories (title,parent_category_id) values (?,?)"
      (title, parentCategoryID)
  pure ()

rewriteCategory :: Connection -> API.EditCategoryRequest -> IO ()
rewriteCategory conn API.EditCategoryRequest {..} = do
  _ <- execTitle newTitle
  _ <- execParent newParentCategoryID
  pure ()
  where
    execTitle (Just t) = execute conn "UPDATE categories SET title = ? WHERE id = ?" (t, processedCategoryID)
    execTitle Nothing = pure 0

    execParent (Just 0) = execute conn "UPDATE categories SET parent_category_id = NULL WHERE id = ?" (Only processedCategoryID)
    execParent (Just parID) = execute conn "UPDATE categories SET parent_category_id = ? WHERE id = ?" (parID, processedCategoryID)
    execParent Nothing = pure 0
