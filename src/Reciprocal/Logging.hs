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

stdioLogger :: (MonadIO m) => Logger m
stdioLogger = Logger
  { loggerWarning = liftIO . putStrLn
  }

logWarning :: Logger m -> Text -> m ()
logWarning = loggerWarning
