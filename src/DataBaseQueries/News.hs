module DataBaseQueries.News where

import Types.Domain.User
import Types.Domain.News
import Types.Domain.Picture
import Types.Domain.Category
import qualified Types.Database.News as DBType
import qualified Types.API.News as API
import qualified Types.Database.Category as DBType
import DataBaseQueries.Auth (getUserIDWhithAuth)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Types
import qualified Data.Text as T
import Control.Monad (mapM)
import qualified Data.ByteString.Char8 as BS

parseNews :: Connection -> [(BS.ByteString, Maybe BS.ByteString)] -> IO [(DBType.News)]
parseNews conn queryList = do
    let initQ = Query "SELECT * FROM news"
    let filters = findFilters queryList
    let sorts = findSorts queryList
    let q = addSorting' sorts $ addFilter' filters initQ
    res <- query_ conn q
    return res

findFilters :: [(BS.ByteString, Maybe BS.ByteString)] -> [(BS.ByteString, Maybe BS.ByteString)]
findFilters ls = filter (\(poleName, _) -> poleName `elem` lislistOfAllowedFilters) ls
    where lislistOfAllowedFilters = 
            [   "creator_id",
                "category_id",
                "created_at",
                "created_until",
                "created_since"
            ]

findSorts ::  [(BS.ByteString, Maybe BS.ByteString)] -> [(BS.ByteString, Maybe BS.ByteString)]
findSorts ls = filter (\(poleName, _) -> poleName == "sort_by") ls

addFilter' :: [(BS.ByteString, Maybe BS.ByteString)] -> Query -> Query
addFilter' [] q = q
addFilter' ls q = q <> " WHERE " <> (Query $ BS.intercalate " AND " $ foldr addParam [] ls)
    where
        addParam (_, Nothing) acc = acc
        addParam ("creator_id", Just val) acc = ("creator_id" <> " = " <> val) : acc
        addParam ("category_id", Just val) acc = ("category_id" <> " = " <> val) : acc
        addParam ("created_at", Just val) acc = ("create_date" <> " = '" <> val <> "'::timestamp") : acc
        addParam ("created_until", Just val) acc = ("create_date" <> " < '" <> val <> "'::timestamp") : acc
        addParam ("created_since", Just val) acc = ("create_date" <> " >= '" <> val <> "'::timestamp") : acc

addSorting' :: [(BS.ByteString, Maybe BS.ByteString)] -> Query -> Query
addSorting' [] q  = q
addSorting' [("sort_by", Just val)] q = q <> " ORDER BY " <> Query val

parseNewsForAutors :: Connection -> Int -> IO [(DBType.News)]
parseNewsForAutors conn userID = do
    res <- query conn "SELECT * FROM news WHERE is_published = true OR creator_id = ?" (Only userID)
    return res

parseNewsPublished :: Connection -> Maybe Query -> Maybe Query -> IO [(DBType.News)]
parseNewsPublished conn startFilterVal startSortVal = do
    let initQuery = Query $ "SELECT * FROM news"
    let filterVal = (Just (Query " is_published = true ")) <> startFilterVal
    let sortVal = startSortVal
    let q = (initQuery `addFilter` filterVal) `addSort` sortVal
    print q                                                             --log
    res <- query_ conn q
    return res

writeNews :: Connection -> News -> IO ()
writeNews conn News {..} = do
    let q = "INSERT INTO news (title, create_date, creator_id, category_id, text_content, is_published) values (?,?,?,?,?,?) RETURNING id"
    [Only newId] <- query conn q (title, createDate, 1 :: Int, (categoryID category), textContent, isPublished) :: IO [Only Int]  --HARD_CODE
    case picturesArray of
        Nothing -> return ()
        Just picArr -> do
            print picArr
            mapM ( \Picture {..} -> do
                let q = "INSERT INTO pictures (base64) values (?) RETURNING id"
                [Only picID] <- query conn q (Only base64) :: IO [Only Int]
                let q' = "INSERT INTO news_pictures (news_id, picture_id) values (?,?)"
                execute conn q' (newId, picID)            
                ) picArr
            return ()

rewriteNews :: Connection -> API.EditNewsRequest -> IO ()
rewriteNews conn API.EditNewsRequest {..} = do
    editTitle <- execTitle newTitle
    editCategory <- execCategoryID newCategoryID
    editText <- execTextContent newTextContent
    editPictures <- execPicturesArray newPicturesArray
    return ()
    where 
        execTitle (Just tit) = execute conn "UPDATE news SET title = ? WHERE id = ?" (tit, newsID)
        execTitle Nothing = pure 0

        execCategoryID (Just cat) = execute conn "UPDATE news SET category_id = ? WHERE id = ?" (cat, newsID)
        execCategoryID Nothing = pure 0

        execTextContent (Just txt) = execute conn "UPDATE news SET text_content = ? WHERE id = ?" (txt, newsID)
        execTextContent Nothing = pure 0

        execPicturesArray (Just picArr) = do
            execute conn "DELETE * FROM news_pictures WHERE news_id = ?"
                    (Only newsID)
            mapM ( \(Picture b64) -> 
                execute conn "UPDATE news_pictures SET pictures_id = ? WHERE news_id = ?"
                        (b64, newsID)
                ) picArr
        execPicturesArray Nothing = pure []

addSort :: Query -> Maybe Query -> Query
addSort q Nothing = q
addSort q (Just sortVal) = q <> " ORDER BY " <> sortVal

addFilter :: Query -> Maybe Query -> Query
addFilter q Nothing = q
addFilter q (Just filterVal) = q <> " WHERE " <> filterVal