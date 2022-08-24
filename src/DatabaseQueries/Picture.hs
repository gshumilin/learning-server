module DatabaseQueries.Picture where

import Types.Domain.Picture
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import qualified Data.Text as T

instance FromRow Int where
    fromRow = field

parsePicturesLinks :: Connection -> Int -> IO (Maybe [T.Text])
parsePicturesLinks conn newsID = do
    let q = "SELECT picture_id FROM news_pictures WHERE news_id = ?"
    res <- query conn q $ Only newsID :: IO ([Int])
    if null $ res
        then return Nothing
        else return . Just $ map makeLinks res
    where
        makeLinks :: Int -> T.Text
        makeLinks picId = "localhost:3000/getPicture?id=" <> (T.pack . show $ picId)

readPicture :: Connection -> Int -> IO (Maybe Picture)
readPicture conn pictureID = do
    let q = "SELECT data, mime FROM pictures WHERE id = ?"
    res <- query conn q $ Only pictureID
    case res of
        [] -> return Nothing
        [x] -> return $ Just x

writePicture :: Connection -> Picture -> IO ()
writePicture conn Picture {..} = do
    execute conn "INSERT INTO pictures (data,mime) VALUES (?,?)" (picData,mime)
    return ()

addPicturesToNews :: Connection -> Int -> [Picture] -> IO ()
addPicturesToNews conn newsId picArr = do
    mapM ( \Picture {..} -> do
        let q = "INSERT INTO pictures (data,mime) values (?,?) RETURNING id"
        [Only picID] <- query conn q (picData, mime) :: IO [Only Int]
        let q' = "INSERT INTO news_pictures (news_id, picture_id) values (?,?)"
        execute conn q' (newsId, picID)            
        ) picArr
    return ()

deleteNewsPictures :: Connection -> Int -> IO ()
deleteNewsPictures conn newsId = do
    picIdArray <- query conn "DELETE FROM news_pictures WHERE news_id=? RETURNING picture_id" (Only newsId) :: IO [Int]
    mapM ( \picID -> execute conn "DELETE FROM pictures WHERE id=?" (Only picID)) picIdArray
    return ()