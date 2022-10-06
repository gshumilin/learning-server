module DatabaseQueries.Picture where

import Control.Monad.Reader (ReaderT, asks)
import qualified Data.Text as T
import Database.PostgreSQL.Simple (Only (..), execute, query)
import Types.Domain.Environment (Environment (..))
import Types.Domain.Picture (Picture (..))
import Utils.Pool (withPool)

parsePicturesLinks :: Int -> ReaderT Environment IO (Maybe [T.Text])
parsePicturesLinks newsId = do
  let q = "SELECT picture_id FROM news_pictures WHERE news_id = ?"
  res <- withPool $ \conn -> query conn q $ Only newsId :: IO [Only Int]
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

readPicture :: Int -> ReaderT Environment IO (Maybe Picture)
readPicture pictureId = do
  let q = "SELECT data, mime FROM pictures WHERE id = ?"
  res <- withPool $ \conn -> query conn q $ Only pictureId
  case res of
    [] -> pure Nothing
    (x : _) -> pure $ Just x

addPicturesToNews :: Int -> [Picture] -> ReaderT Environment IO ()
addPicturesToNews newsId picArr = do
  mapM_
    ( \Picture {..} -> do
        let q = "INSERT INTO pictures (data,mime) values (?,?) RETURNING id"
        [Only picId] <- withPool $ \conn -> query conn q (picData, mime) :: IO [Only Int]
        let q' = "INSERT INTO news_pictures (news_id, picture_id) values (?,?)"
        withPool $ \conn -> execute conn q' (newsId, picId)
    )
    picArr

deleteNewsPictures :: Int -> ReaderT Environment IO ()
deleteNewsPictures newsId = do
  picIdArray <- withPool $ \conn ->
    query conn "DELETE FROM news_pictures WHERE news_id=? RETURNING picture_id" (Only newsId) :: IO [Only Int]
  mapM_ (\i -> withPool $ \conn -> execute conn "DELETE FROM pictures WHERE id=?" i) picIdArray
