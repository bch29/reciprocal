module Reciprocal.Logging
  (
  -- * Types
    Logger
  -- * Logging methods
  , logWarning
  -- * Concrete loggers
  , stdioLogger
  ) where

import Reciprocal.Prelude

import Data.Text.IO (putStrLn)

data Logger m = Logger
  { loggerWarning :: Text -> m ()
  }

stdioLogger :: Logger IO
stdioLogger = Logger
  { loggerWarning = putStrLn
  }

logWarning :: Logger m -> Text -> m ()
logWarning = loggerWarning
