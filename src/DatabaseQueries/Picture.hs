module DatabaseQueries.Picture where

import Types.Domain.Picture
import Database.PostgreSQL.Simple
import qualified Data.Text as T

findPicturesArray :: Connection -> Int -> IO (Maybe [Picture])
findPicturesArray conn newsID = do
    let q = "SELECT p.picData FROM news_pictures np LEFT JOIN pictures p on p.id=np.picture_id WHERE np.news_id = ?"
    res <- query conn q $ Only newsID
    if not . null $ res
        then return $ Just res
        else return Nothing

readPicture :: Connection -> Int -> IO (Maybe Picture)
readPicture conn pictureID = do
    let q = "SELECT picData FROM pictures WHERE id = ?"
    res <- query conn q $ Only pictureID
    case res of
        [] -> return Nothing
        [x] -> return $ Just x

writePicture :: Connection -> Picture -> IO ()
writePicture conn Picture {..} = do
    execute conn "INSERT INTO pictures (picData) VALUES (?)" (Only picData)
    return ()