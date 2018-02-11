{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module Reciprocal.DB
  (
    -- * Types
    DB

    -- * Opening a 'DB'
  , openDB

    -- * Storing and loading objects
  , StorableDB(..)
  ) where

import           Reciprocal.Prelude

import           Reciprocal.Config
import           Reciprocal.Model.Ingredient
import           Reciprocal.Model.Recipe

import System.Directory (createDirectoryIfMissing)

-- TODO: This is just an abstract interface!

data DB = DB
  { _dbRootDir :: FilePath
  }

makeFields ''DB

-- | Opens the database, and creates it if it doesn't exist. Throws an exception
-- if some file system error was encountered while opening the database.
openDB :: Config -> IO DB
openDB cfg = do
  let _dbRootDir = cfg ^. rootDir
  createDirectoryIfMissing True _dbRootDir
  return (DB { _dbRootDir })

class StorableDB a where
  -- | Extracts the name of the object, which is used to key searches.
  storableName :: a -> Text

  -- | Stores the object in the database. Returns 'True' on success, or 'False'
  -- if an object with the same name was already in the database. Throws
  -- exceptions on other types of error.
  store :: DB -> a -> IO Bool

  -- | Returns the object with the exact given name from the database, if it
  -- exists. Returns 'Nothing' if no such object exists. Throws exceptions on other
  -- types of error.
  load :: DB -> Text -> IO (Maybe a)

  -- | Searches for an object in the database, with the given search string.
  -- Returns all matching objects as a stream.
  find :: DB -> Text -> Stream IO (Of a) ()


--------------------------------------------------------------------------------
--  'StorableDB' Instances
--------------------------------------------------------------------------------

instance StorableDB Ingredient where
  storableName = view name
  store = undefined
  load = undefined
  find = undefined

instance StorableDB Recipe where
  storableName = view title
  store = undefined
  load = undefined
  find = undefined

--------------------------------------------------------------------------------
--  Other Instances
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
--  Internal
--------------------------------------------------------------------------------

-- dbFilePath :: DBName -> IO FilePath
-- dbFilePath (DBName x)
