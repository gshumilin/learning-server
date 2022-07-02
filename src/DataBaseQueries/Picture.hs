module DataBaseQueries.Picture where

import Types.Domain.Picture
import Database.PostgreSQL.Simple

findPicturesArray :: Connection -> Integer -> IO (PicturesArray)
findPicturesArray conn pictureID = do
    res <- query conn "SELECT base64 FROM pictures WHERE id = ?" $ Only pictureID
    return $ PicturesArray res