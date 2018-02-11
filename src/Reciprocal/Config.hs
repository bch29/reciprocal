{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}

module Reciprocal.Config where

import Reciprocal.Prelude

import System.Environment (getEnv)
import System.FilePath ((</>))

-- | The default root directory to store configuration in, relative to the
-- user's home directory.
defaultRootDir :: FilePath
defaultRootDir = ".config/reciprocal"


data Config = Config
  { _configRootDir :: FilePath
  }



makeFields ''Config


defaultConfig :: IO Config
defaultConfig = do
  homeDir <- getEnv "HOME"

  return $ Config
    { _configRootDir = homeDir </> defaultRootDir
    }
