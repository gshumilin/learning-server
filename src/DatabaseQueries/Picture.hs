module DatabaseQueries.Picture where

import Control.Monad.Reader (ReaderT, asks, lift)
import qualified Data.Text as T
import Database.PostgreSQL.Simple (Connection, Only (..), execute, query)
import Types.Domain.Environment (Environment (..))
import Types.Domain.Picture (Picture (..))
import Utils (askConnection)

parsePicturesLinks :: Int -> ReaderT Environment IO (Maybe [T.Text])
parsePicturesLinks newsId = do
  conn <- askConnection
  let q = "SELECT picture_id FROM news_pictures WHERE news_id = ?"
  res <- lift $ query conn q $ Only newsId :: ReaderT Environment IO [Only Int]
  if null res
    then pure Nothing
    else do
      linksList <- mapM makeLinks res
      pure $ Just linksList
  where
    makeLinks :: Only Int -> ReaderT Environment IO T.Text
    makeLinks (Only picId) = do
      domainInfo <- asks domain
      pure $ domainInfo <> "/picture?id=" <> (T.pack . show $ picId)

readPicture :: Connection -> Int -> IO (Maybe Picture)
readPicture conn pictureId = do
  let q = "SELECT data, mime FROM pictures WHERE id = ?"
  res <- query conn q $ Only pictureId
  case res of
    [] -> pure Nothing
    (x : _) -> pure $ Just x

addPicturesToNews :: Connection -> Int -> [Picture] -> IO ()
addPicturesToNews conn newsId picArr = do
  mapM_
    ( \Picture {..} -> do
        let q = "INSERT INTO pictures (data,mime) values (?,?) RETURNING id"
        [Only picId] <- query conn q (picData, mime) :: IO [Only Int]
        let q' = "INSERT INTO news_pictures (news_id, picture_id) values (?,?)"
        execute conn q' (newsId, picId)
    )
    picArr

deleteNewsPictures :: Connection -> Int -> IO ()
deleteNewsPictures conn newsId = do
  picIdArray <- query conn "DELETE FROM news_pictures WHERE news_id=? RETURNING picture_id" (Only newsId) :: IO [Only Int]
  mapM_ (execute conn "DELETE FROM pictures WHERE id=?") picIdArray
