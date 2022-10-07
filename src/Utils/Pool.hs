module Utils.Pool where

import Control.Monad.Reader (ReaderT, asks, lift)
import Data.Pool (withResource)
import Database.PostgreSQL.Simple (Connection)
import Types.Domain.Environment (Environment (..))

withPool :: (Connection -> IO a) -> ReaderT Environment IO a
withPool f = do
  pool <- asks dbPool
  lift $ withResource pool f
