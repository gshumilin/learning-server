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
    --программа игнорирует пустые поля и поля с некорректными названиями фильтров и параметров сортировки в запросе.
    let filtersQuery = makeFiltersQuery . findCompletedFields [ "creator_login", "category_title", "created_at", "created_until", "created_since"] . queryString $ req
    let sortsQuery = makeSimpleQuery . findCompletedFields ["sort_by"] . queryString $ req 
    let limitQuery = makeSimpleQuery . findCompletedFields ["limit"] . queryString $ req 
    let offsetQuery = makeSimpleQuery . findCompletedFields ["offset"] . queryString $ req
    let resultQ = Just initQuery <> filtersQuery <> sortsQuery <> limitQuery <> offsetQuery
    return resultQ  

findCompletedFields :: [BS.ByteString] -> [(BS.ByteString, Maybe BS.ByteString)] -> [(BS.ByteString, Maybe BS.ByteString)]
findCompletedFields allowedNames ls = filter (\(fieldName, val) -> fieldName `elem` allowedNames && isJust val) ls

makeInitReadNewsQuery :: Either T.Text DBType.User -> Query
makeInitReadNewsQuery (Left _) = 
    Query $    "SELECT  news.id,\n" <>
               "         news.title,\n" <>
               "         news.create_date,\n" <>
               "         news.creator_id,\n" <>
               "         users.login,\n" <>
               "         news.category_id,\n" <>
               "         categories.title,\n" <>
               "         news.text_content,\n" <>
               "         news.is_published\n" <>
               " FROM    news\n" <>
               " JOIN    users on news.creator_id=users.id\n" <>
               " JOIN    categories on news.category_id=categories.id\n" <>
               " WHERE   is_published = True"
makeInitReadNewsQuery (Right DBType.User {..}) = 
    Query $    "SELECT  news.id,\n" <>
               "         news.title,\n" <>
               "         news.create_date,\n" <>
               "         news.creator_id,\n" <>
               "         users.login,\n" <>
               "         news.category_id,\n" <>
               "         categories.title,\n" <>
               "         news.text_content,\n" <>
               "         news.is_published\n" <>
               " FROM    news\n" <>
               " JOIN    users on news.creator_id=users.id\n" <>
               " JOIN    categories on news.category_id=categories.id\n" <>
               " WHERE (is_published = True OR creator_id = " <> BS.pack (show userID) <> ")"

makeFiltersQuery :: [(BS.ByteString, Maybe BS.ByteString)] -> Maybe Query
makeFiltersQuery [] = Nothing
makeFiltersQuery ls = Just $ Query " AND " <> (Query $ BS.intercalate " AND " $ foldr addParam [] ls)
    where
        addParam ("is_published", Just val) acc     = ("is_published" <> " = " <> val) : acc
        addParam ("creator_id", Just val) acc       = ("creator_id" <> " = " <> val) : acc
        addParam ("creator_login", Just val) acc    = ("users.login" <> " = '" <> val <> "'") : acc
        addParam ("category_id", Just val) acc      = ("category_id" <> " = " <> val) : acc
        addParam ("category_title", Just val) acc   = ("categories.title" <> " = '" <> val <> "'") : acc
        addParam ("created_at", Just val) acc       = ("create_date" <> " = '" <> val <> "'::timestamp") : acc
        addParam ("created_until", Just val) acc    = ("create_date" <> " < '" <> val <> "'::timestamp") : acc
        addParam ("created_since", Just val) acc    = ("create_date" <> " >= '" <> val <> "'::timestamp") : acc

makeSimpleQuery :: [(BS.ByteString, Maybe BS.ByteString)] -> Maybe Query
makeSimpleQuery [] = Nothing
makeSimpleQuery [("limit", Just val)]   = Just $ " LIMIT "    <> Query val
makeSimpleQuery [("offset", Just val)]  = Just $ " OFFSET "   <> Query val
makeSimpleQuery [("sort_by", Just "creator_login")] = Just $ " ORDER BY users.login"
makeSimpleQuery [("sort_by", Just "category_title")] = Just $ " ORDER BY categories.title"
makeSimpleQuery [("sort_by", Just "create_date")] = Just $ " ORDER BY create_date"
makeSimpleQuery [("sort_by", Just _)] = Nothing