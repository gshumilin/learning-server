module Log where

import Types.Domain.Log (LogLvl(..))
import Types.Domain.Environment
import qualified Data.Text as T
import Data.Text.Encoding as T
import Control.Monad.Reader

addLog :: LogLvl ->  String -> ReaderT Environment IO ()
addLog logLvl log = do undefined 
    -- logPath' <- asks confLogPath
    -- let logPath = T.unpack logPath'
    -- lift $ putStrLn log
    -- lift $ appendFile logPath (log ++ "\n") 