module Log where

import Control.Monad.Reader (ReaderT, ask, lift)
import qualified Data.Text as T (Text, pack)
import Data.Text.IO (hPutStrLn)
import Data.Time (getCurrentTime)
import System.IO (Handle, IOMode (AppendMode), openFile, stderr, stdout)
import Types.Domain.Environment (Environment (..))
import Types.Domain.Log (LogDescType (..), LogLvl (..))

addLog :: LogLvl -> T.Text -> ReaderT Environment IO ()
addLog lvl logMsg = do
  Environment {..} <- ask
  if lvl >= logLvl
    then do
      time <- lift getCurrentTime
      let msg = T.pack (show time) <> " :: " <> T.pack (show lvl) <> " :: " <> logMsg
      lift $ hPutStrLn logDesc msg
    else pure ()

makeLogDesc :: LogDescType -> IO Handle
makeLogDesc (LogFile logFilePath) = openFile logFilePath AppendMode
makeLogDesc StdErr = pure stderr
makeLogDesc StdOut = pure stdout
