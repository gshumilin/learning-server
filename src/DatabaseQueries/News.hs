module DatabaseQueries.News where

import Control.Monad.Reader
import qualified Data.ByteString.Char8 as BS
import Data.Maybe (fromMaybe)
import Data.Time
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Types
import DatabaseQueries.Picture (addPicturesToNews, deleteNewsPictures, parsePicturesLinks)
import DatabaseQueries.QueryCreator (makeReadNewsQuery)
import DatabaseQueries.User (findUser)
import Endpoints.Categories (getSpecificCategory)
import Log (addLog)
import Network.Wai
import qualified Types.API.News as API
import qualified Types.Database.News as DBType
import Types.Domain.Environment
import Types.Domain.Log
import qualified Types.Domain.News as Domain
import qualified Types.Domain.Picture as Domain

readNews :: Request -> ReaderT Environment IO [Domain.News]
readNews req = do
  addLog DEBUG "----- Started readNews \n"
  conn <- asks dbConnection
  mbQuery <- lift $ makeReadNewsQuery conn req
  case mbQuery of
    Nothing -> pure []
    Just q -> do
      let (Query bsQ) = q
      addLog DEBUG $ "----- made this psql-request: \n\"" ++ BS.unpack bsQ ++ "\"\n"
      dbNews <- lift $ query_ conn q :: ReaderT Environment IO [DBType.News]
      addLog DEBUG $ "----- got this psql News List: \"" ++ show dbNews ++ "\"\n"
      lift $ mapM (fromDbNews conn) dbNews

fromDbNews :: Connection -> DBType.News -> IO Domain.News
fromDbNews conn DBType.News {..} = do
  newsCategory <- getSpecificCategory conn categoryID
  newsCreator <- findUser conn creatorID
  newsPictures <- parsePicturesLinks conn newsID
  pure $
    Domain.News
      { newsID = newsID,
        title = title,
        createDate = createDate,
        creator = newsCreator,
        category = newsCategory,
        textContent = textContent,
        picturesLinks = newsPictures,
        isPublished = isPublished,
        numbersOfPictures = numbersOfPictures
      }

readSpecificNews :: Connection -> Int -> IO (Maybe DBType.EditedNewsFields)
readSpecificNews conn newsID = do
  let q = "SELECT creator_id, title, category_id, text_content FROM news WHERE id=?"
  res <- query conn q (Only newsID) :: IO [DBType.EditedNewsFields]
  case res of
    [] -> pure Nothing
    [news] -> pure $ Just news
    (news : _) -> pure $ Just news

writeNews :: Connection -> Int -> API.CreateNewsRequest -> IO ()
writeNews conn newsCreatorID API.CreateNewsRequest {..} = do
  currTime <- getCurrentTime
  let isPublished = False
  let q = "INSERT INTO news (title, create_date, creator_id, category_id, text_content, is_published) values (?,?,?,?,?,?) pureING id"
  [Only newsId] <- query conn q (title, currTime, newsCreatorID, categoryID, textContent, isPublished) :: IO [Only Int]
  case pictures of
    Nothing -> pure ()
    Just picArr -> do
      mapM_
        ( \Domain.Picture {..} -> do
            let insertingQ = "INSERT INTO pictures (data,mime) values (?,?) pureING id"
            [Only picID] <- query conn insertingQ (picData, mime) :: IO [Only Int]
            let insertingQ' = "INSERT INTO news_pictures (news_id, picture_id) values (?,?)"
            execute conn insertingQ' (newsId, picID)
        )
        picArr

rewriteNews :: Connection -> DBType.EditedNewsFields -> API.EditNewsRequest -> IO ()
rewriteNews conn editedNewsFields editNewsRequest = do
  let q = "UPDATE news SET title=?, category_id=?, text_content=? WHERE id=?"
  _ <-
    execute
      conn
      q
      ( fromMaybe (DBType.oldTitle editedNewsFields) (API.newTitle editNewsRequest),
        fromMaybe (DBType.oldCategoryID editedNewsFields) (API.newCategoryID editNewsRequest),
        fromMaybe (DBType.oldTextContent editedNewsFields) (API.newTextContent editNewsRequest),
        API.newsID editNewsRequest
      )
  case API.newPictures editNewsRequest of
    Nothing -> pure ()
    Just [] -> do
      deleteNewsPictures conn (API.newsID editNewsRequest)
      pure ()
    Just picArr -> do
      deleteNewsPictures conn (API.newsID editNewsRequest)
      addPicturesToNews conn (API.newsID editNewsRequest) picArr
      pure ()
