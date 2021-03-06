module DatabaseQueries.News where

import qualified Types.Domain.User as Domain
import qualified Types.Domain.News as Domain
import qualified Types.Domain.Picture as Domain
import qualified Types.Domain.Category as Domain
import Types.Domain.Environment
import qualified Types.Database.News as DBType
import qualified Types.API.News as API
import qualified Types.Database.Category as DBType
import Endpoints.Categories (getSpecificCategory)
import DatabaseQueries.User (findUser)
import DatabaseQueries.Picture (findPicturesArray)
import DatabaseQueries.Auth (getUserIdWithAuth)
import Auth
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Types
import qualified Data.Text as T
import Data.Time
import Control.Monad.Reader
import qualified Data.ByteString.Char8 as BS
import Network.Wai

parseNews :: Request -> ReaderT Environment IO [Domain.News]
parseNews req = do
    conn <- asks dbConnection
    let queryList = queryString req
    let authInfo = findAuthKey req
    mbClientUserID <- lift $ getUserIdWithAuth conn (decodeAuthKey =<< authInfo) 
    let filters = findFilters queryList  
    let sorts = findSorts queryList         
    let initQ = 
            case mbClientUserID of
                Nothing -> Query "SELECT * FROM news WHERE is_published = True"
                Just clientUserID -> 
                    if any (\(k,v) -> k=="creator_id") filters 
                        then Query "SELECT * FROM news WHERE is_published = True"
                        else Query "SELECT * FROM news WHERE is_published = True OR creator_id = " <> Query (BS.pack $ show clientUserID) 
    let q = addSorting sorts $ addFilter filters initQ
    lift . putStrLn $ "----- created this psql-request: \"" ++ (show q) ++ "\"\n" --log
    dbNews <- lift $ query_ conn q :: ReaderT Environment IO [DBType.News]
    res <- lift $ mapM (fromDbNews conn) dbNews
    return res

fromDbNews :: Connection -> DBType.News -> IO Domain.News
fromDbNews conn DBType.News {..} = do 
    newsCategory <- getSpecificCategory conn categoryID             --refactoring!
    newsCreator <- findUser conn creatorID              --refactoring! unsafe 'head' is used
    newsPicturesArray <- findPicturesArray conn newsID
    return $ Domain.News 
        {   newsID = newsID,
            title = title,
            createDate = createDate,
            creator = newsCreator,
            category = newsCategory,
            textContent = textContent,
            picturesArray = newsPicturesArray,
            isPublished = isPublished 
        }

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

addFilter :: [(BS.ByteString, Maybe BS.ByteString)] -> Query -> Query
addFilter [] q = q
addFilter ls q = q <> " AND " <> (Query $ BS.intercalate " AND " $ foldr addParam [] ls)
    where
        addParam (_, Nothing) acc = acc
        addParam ("is_published", Just val) acc = ("is_published" <> " = " <> val) : acc
        addParam ("creator_id", Just val) acc = ("creator_id" <> " = " <> val) : acc
        addParam ("category_id", Just val) acc = ("category_id" <> " = " <> val) : acc
        addParam ("created_at", Just val) acc = ("create_date" <> " = '" <> val <> "'::timestamp") : acc
        addParam ("created_until", Just val) acc = ("create_date" <> " < '" <> val <> "'::timestamp") : acc
        addParam ("created_since", Just val) acc = ("create_date" <> " >= '" <> val <> "'::timestamp") : acc


addSorting :: [(BS.ByteString, Maybe BS.ByteString)] -> Query -> Query
addSorting [] q  = q
addSorting [("sort_by", Just val)] q = q <> " ORDER BY " <> Query val

writeNews :: Connection -> API.CreateNewsRequest -> IO ()
writeNews conn API.CreateNewsRequest {..} = do
    let newsCreatorID = 1 :: Int
    currTime <- getCurrentTime
    let isPublished = False
    let q = "INSERT INTO news (title, create_date, creator_id, category_id, text_content, is_published) values (?,?,?,?,?,?) RETURNING id"
    [Only newId] <- query conn q (title, currTime, newsCreatorID, categoryID, textContent, isPublished) :: IO [Only Int]
    case picturesArray of
        Nothing -> return ()
        Just picArr -> do
            print picArr
            mapM ( \Domain.Picture {..} -> do
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
            mapM ( \(Domain.Picture b64) -> 
                execute conn "UPDATE news_pictures SET pictures_id = ? WHERE news_id = ?"
                        (b64, newsID)
                ) picArr
        execPicturesArray Nothing = pure []

xs = [  ("creator_id", Just "1"),
        ("category_id", Nothing), 
        ("created_since", Just "2022-12-21")
     ] :: [(BS.ByteString, Maybe BS.ByteString)]

ys = [] :: [(BS.ByteString, Maybe BS.ByteString)]

zs = [  ("creator_id", Just "1"), 
        ("category_id", Nothing), 
        ("created_since", Just "2022-12-21"), 
        ("created_since", Just "2022-12-21")
     ] :: [(BS.ByteString, Maybe BS.ByteString)]

initQ = Query "SELECT * FROM news WHERE is_published=True OR creator_id=1"

addFilter' :: [(BS.ByteString, Maybe BS.ByteString)] -> Query -> Query
addFilter' [] q = q
addFilter' ls q = q <> " AND " <> (Query $ BS.intercalate " AND " $ foldr addParam [] ls)
    where
        addParam (_, Nothing) acc = acc
        addParam ("is_published", Just val) acc = ("is_published" <> " = " <> val) : acc
        addParam ("creator_id", Just val) acc = ("creator_id" <> " = " <> val) : acc
        addParam ("category_id", Just val) acc = ("category_id" <> " = " <> val) : acc
        addParam ("created_at", Just val) acc = ("create_date" <> " = '" <> val <> "'::timestamp") : acc
        addParam ("created_until", Just val) acc = ("create_date" <> " < '" <> val <> "'::timestamp") : acc
        addParam ("created_since", Just val) acc = ("create_date" <> " >= '" <> val <> "'::timestamp") : acc