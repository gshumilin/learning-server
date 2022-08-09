module DatabaseQueries.QueryCreator where

import qualified Types.Domain.News as Domain
import Types.Domain.Environment
import qualified Types.Database.News as DBType
import qualified Types.Database.User as DBType
import Endpoints.Categories (getSpecificCategory)
import DatabaseQueries.User (findUser, findUserIdByLogin)
import DatabaseQueries.Category (findCategoryIdByTitle)
import DatabaseQueries.Picture (findPicturesArray)
import Auth (authorization)
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BS
import Control.Monad.Reader
import Data.Maybe (isJust, isNothing)
import Network.Wai (Request)
import Database.PostgreSQL.Simple (Connection(..))
import Database.PostgreSQL.Simple.Types (Query(..))
import Network.Wai (queryString)

-- " SELECT * FROM news WHERE is_published = True AND creator_id = 1 AND create_since >= 2022-12-21 "

-- 1. Человек вышел из воды и прислал мне Req (там могут быть фильтры, сорты, лимиты, оффсеты, авторизация)
-- 2. Авторзовываем человека и записываем в authInfo = Either T.Text DBType.User
-- 3. Создаём initQuery (он зависит от authInfo). 
--      Если Nothing, то               ->       "SELECT * FROM news WHERE is_published = True"
--      Если Just DBType.User {..}, то ->       "SELECT * FROM news WHERE (is_published = True OR creator_id = 1)"
-- 4. Добавляем параметры к initQuery

makeReadNewsQuery :: Connection -> Request -> IO (Maybe Query)  
makeReadNewsQuery conn req = do
    clientUser <- authorization conn req
    --стартовый вариант запроса зависит от того, авторизован ли пользователь. 
    let initQuery = makeInitReadNewsQuery clientUser
    --дальше нужно добавить к стартовому запросу фильтры, сорты, лимит и оффсет. 
    --программа игнорирует пустые поля и поля с некорректными названиями фильтров в запросе. Отсекаем их. Потом заменяем некоторые из полей на Maybe id
    filtersList <-  replaceWithId conn 
                  . findCompletedFields [ "creator_login", "category_title", "created_at", "created_until", "created_since"]
                  . queryString $ req  
    --теперь, если хоть одно из значений фильтров будет Nothing, то запрос новостей в любом случае выдаст пустой список...
    -- ...потому что фиьтрация по несуществующему пользователю или категории приведёт к пустому списку
    if not . null $ filter (\(_, val)-> isNothing val) filtersList
        then return Nothing
    --если вернём Nothing, функция DatabaseQueries.News.readNews вернёт пустой список новостей
        else do
            let filtersQuery = makeFiltersQuery filtersList 
            let sortsQuery = makeSimpleQuery . findCompletedFields ["sort_by"] . queryString $ req 
            let limitQuery = makeSimpleQuery . findCompletedFields ["limit"] . queryString $ req 
            let offsetQuery = makeSimpleQuery . findCompletedFields ["offset"] . queryString $ req
            let resultQ = Just initQuery <> filtersQuery <> sortsQuery <> limitQuery <> offsetQuery
            return resultQ  

findCompletedFields :: [BS.ByteString] -> [(BS.ByteString, Maybe BS.ByteString)] -> [(BS.ByteString, Maybe BS.ByteString)]
findCompletedFields allowedNames ls = filter (\(fieldName, val) -> fieldName `elem` allowedNames && isJust val) ls

makeInitReadNewsQuery :: Either T.Text DBType.User -> Query
makeInitReadNewsQuery (Left _) = 
    Query "SELECT * FROM news WHERE is_published = True"
makeInitReadNewsQuery (Right DBType.User {..}) = 
    Query "SELECT * FROM news WHERE (is_published = True OR creator_id = " <> Query (BS.pack (show userID)) <> Query ")"

makeFiltersQuery :: [(BS.ByteString, Maybe BS.ByteString)] -> Maybe Query
makeFiltersQuery [] = Nothing
makeFiltersQuery ls = Just $ Query " AND " <> (Query $ BS.intercalate " AND " $ foldr addParam [] ls)
    where
        addParam ("is_published", Just val) acc = ("is_published" <> " = " <> val) : acc
        addParam ("creator_id", Just val) acc = ("creator_id" <> " = " <> val) : acc
        addParam ("category_id", Just val) acc = ("category_id" <> " = " <> val) : acc
        addParam ("created_at", Just val) acc = ("create_date" <> " = '" <> val <> "'::timestamp") : acc
        addParam ("created_until", Just val) acc = ("create_date" <> " < '" <> val <> "'::timestamp") : acc
        addParam ("created_since", Just val) acc = ("create_date" <> " >= '" <> val <> "'::timestamp") : acc

makeSimpleQuery :: [(BS.ByteString, Maybe BS.ByteString)] -> Maybe Query
makeSimpleQuery [] = Nothing
makeSimpleQuery [("sort_by", Just val)] = Just $ " ORDER BY " <> Query val
makeSimpleQuery [("limit", Just val)]   = Just $ " LIMIT "    <> Query val
makeSimpleQuery [("offset", Just val)]  = Just $ " OFFSET "   <> Query val

replaceWithId :: Connection -> [(BS.ByteString, Maybe BS.ByteString)] -> IO [(BS.ByteString, Maybe BS.ByteString)] 
replaceWithId conn filtersList = mapM (replacer conn) filtersList
    where
        replacer :: Connection -> (BS.ByteString, Maybe BS.ByteString) -> IO (BS.ByteString, Maybe BS.ByteString)
        replacer conn ("creator_login", Just login) = do
            mbId <- findUserIdByLogin conn login
            return ("creator_id", mbId)
        replacer conn ("category_title", Just title) = do
            mbId <- findCategoryIdByTitle conn title
            return ("category_id", mbId)
        replacer conn someTuple = return someTuple