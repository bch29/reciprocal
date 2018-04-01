module Reciprocal.Config where

import           Reciprocal.Prelude

import           System.Environment (getEnv)
import           System.FilePath    ((</>))

-- | The default root directory to store configuration in, relative to the
-- user's home directory.
defaultRootDir :: FilePath
defaultRootDir = ".config/reciprocal"


data Config = Config
  { rootDir :: FilePath
  }
  deriving (Generic)


defaultConfig :: IO Config
defaultConfig = do
  homeDir <- getEnv "HOME"

  return $ Config
    { rootDir = homeDir </> defaultRootDir
    }
