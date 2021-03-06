module DatabaseQueries.Category where

import Types.Domain.Category
import qualified Types.Database.Category as DBType
import qualified Types.API.Category as API
import Database.PostgreSQL.Simple

parseCategoriesList :: Connection -> IO [DBType.Category]
parseCategoriesList conn = do
    res <- query_ conn "SELECT * FROM categories"
    return res

parseSpecificCategory :: Int -> Connection -> IO [(DBType.Category)]
parseSpecificCategory id conn = do
    let q = "with recursive records_list as (select n.id as id, n.title as title, n.parent_category_id as parent_category_id, 1 as depth from categories as n where id = ? union all select n.id as id, n.title as title, n.parent_category_id as parent_category_id, records_list.depth + 1 as depth from categories as n  join records_list on records_list.parent_category_id = n.id ) select records_list.id, records_list.title, records_list.parent_category_id from records_list order by depth;"
    res <- query conn q (Only id)
    return res

writeCategory :: Connection -> API.CreateCategoryRequest -> IO ()
writeCategory conn API.CreateCategoryRequest {..} = do
    execute conn "INSERT INTO categories (title,parent_category_id) values (?,?)"
                    (title, parentCategoryID)
    return ()

rewriteCategory :: Connection -> API.EditCategoryRequest -> IO ()
rewriteCategory conn API.EditCategoryRequest {..} = do
    editTitle <- execTitle newTitle
    editParent <- execParent newParentCategoryID
    return ()
    where 
        execTitle (Just t) = execute conn "UPDATE categories SET title = ? WHERE id = ?" (newTitle, categoryID)
        execTitle Nothing = pure (0)

        execParent (Just parID) = execute conn "UPDATE categories SET parent_category_id = ? WHERE id = ?" (parID, categoryID)
        execParent Nothing = pure (0) 