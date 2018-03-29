module Reciprocal.Logging
  (
  -- * Types
    Logger
  -- * Logging methods
  , logWarning
  -- * Concrete loggers
  , stdioLogger
  , fileLogger
  ) where

import Reciprocal.Prelude

import Data.Text.IO (putStrLn)

data Logger m = Logger
  { loggerWarning :: Text -> m ()
  }

stdioLogger :: (MonadIO m) => Logger m
stdioLogger = Logger
  { loggerWarning = liftIO . putStrLn
  }

fileLogger :: (MonadIO m) => FilePath -> IO (Logger m)
fileLogger logFile = do
  writeFile logFile ""
  return $ Logger
    { loggerWarning = liftIO . appendFile logFile . (<> "\n")
    }

logWarning :: Logger m -> Text -> m ()
logWarning = loggerWarning
