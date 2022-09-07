module Log where

import Control.Monad.Reader (ReaderT, asks, lift)
import qualified Data.Text as T
import Types.Domain.Environment (Environment (..))
import Types.Domain.Log (LogInfo (..), LogLvl)

addLog :: LogLvl -> String -> ReaderT Environment IO ()
addLog lvl logMsg = do
  LogInfo {..} <- asks logInfo
  lift $ putStrLn logMsg
  if lvl >= logLvl
    then lift $ appendFile (T.unpack logPath) (logMsg ++ "\n")
    else pure ()
