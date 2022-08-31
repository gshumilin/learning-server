module Log where

import Control.Monad.Reader
import qualified Data.Text as T
import Data.Text.Encoding as T
import Types.Domain.Environment (Environment (..))
import Types.Domain.Log

addLog :: LogLvl -> String -> ReaderT Environment IO ()
addLog lvl log = do
  LogInfo {..} <- asks logInfo
  lift $ putStrLn log
  if lvl >= logLvl
    then lift $ appendFile (T.unpack logPath) (log ++ "\n")
    else pure ()
