module DataBaseQueries.Picture where

import Types.Domain.Picture
import Database.PostgreSQL.Simple

findPicturesArray :: Connection -> Int -> IO (Maybe PicturesArray)
findPicturesArray conn pictureID = do
    res <- query conn "SELECT base64 FROM pictures WHERE id = ?" $ Only pictureID
    if not . null $ res
        then return . Just . PicturesArray $ res
        else return Nothing