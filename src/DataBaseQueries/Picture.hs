module DataBaseQueries.Picture where

import Types.Domain.Picture
import Database.PostgreSQL.Simple

findPicturesArray :: Connection -> Integer -> IO (PicturesArray)
findPicturesArray = undefined