module Types.Domain.Log where

import Control.Monad (mzero)
import Data.Aeson.Types (FromJSON, Value (..), parseJSON)
import Data.Text (toUpper)
import qualified Data.Text as T

-- DEBUG    — logging of all events during debugging
-- WARNING  — logging errors and warnings
-- RELEASE  — logging only total errors (for example, "there is no connection to the internet")

data LogLvl = DEBUG | WARNING | RELEASE deriving (Show, Eq, Ord)

instance FromJSON LogLvl where
  parseJSON (String txt)
    | toUpper txt == "DEBUG" = pure DEBUG
    | toUpper txt == "WARNING" = pure WARNING
    | toUpper txt == "RELEASE" = pure RELEASE
    | otherwise = mzero
  parseJSON _ = mzero

data LogDescType = LogFile FilePath | StdErr | StdOut deriving (Show)

instance FromJSON LogDescType where
  parseJSON (String s)
    | s == "stderr" = pure StdErr
    | s == "stdout" = pure StdOut
    | otherwise = pure . LogFile $ T.unpack s
  parseJSON _ = mzero
