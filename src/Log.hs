module Log where

import Types.Domain.Log
import Types.Domain.Environment (Environment(..))
import qualified Data.Text as T
import Data.Text.Encoding as T
import Control.Monad.Reader

addLog :: LogLvl ->  String -> ReaderT Environment IO ()
addLog logLvl log = do
    LogInfo {..} <- asks logInfo
    lift $ putStrLn log
    lift $ appendFile (T.unpack logPath) (log ++ "\n") 