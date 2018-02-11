{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}

module Reciprocal.DB
  (
    -- * Types
    DB
  , LoadError(..)

    -- * Opening a 'DB'
  , openDB

    -- * Storing and loading objects
  , StorableDB(..)
  ) where

import           Reciprocal.Prelude

import           Reciprocal.Config
import           Reciprocal.Model.Ingredient
import           Reciprocal.Model.Recipe

import qualified Data.Aeson                  as Aeson
import qualified Data.ByteString.Lazy        as BS
import           System.Directory            (createDirectoryIfMissing,
                                              doesDirectoryExist, doesFileExist)
import           System.FilePath             ((</>))

import           Control.Monad.Except        (runExceptT, throwError)
import           Data.Foldable               (foldlM)

-- TODO: Searching functions

data DB = DB
  { _dBRootDir :: FilePath
  }

makeFields ''DB

-- | Opens the database, and creates it if it doesn't exist. Throws an exception
-- if some file system error was encountered while opening the database.
openDB :: Config -> IO DB
openDB cfg = do
  let _dBRootDir = cfg ^. rootDir
  createDirectoryIfMissing True _dBRootDir
  return (DB { _dBRootDir })

data LoadError
  = MalformedData String
  | NoSuchObject

class StorableDB a where
  -- | Extracts the name of the object, which is used to key searches.
  storableName :: a -> Text

  -- | Gives the name that objects of this type are stored under.
  storableTypeName :: proxy a -> Text

  -- | Stores the object in the database, overwriting it if it already exists.
  -- Throws exceptions on other types of error.
  store :: DB -> a -> IO ()

  -- | Returns the object with the exact given name from the database, if it
  -- exists. Returns 'Left' if no such object exists or it is malformed. Throws
  -- exceptions on other types of error, i.e. file system errors or malformed
  -- data.
  load :: DB -> Text -> IO (Either LoadError a)

  -- | Searches for an object in the database, with the given search string.
  -- Returns all matching objects as a stream. Objects show up as 'Left' if they
  -- exist but are malformed.
  find :: DB -> Text -> Stream IO (Of (Either String a)) ()

  default store :: (ToJSON a) => DB -> a -> IO ()
  store db x = saveJSONUnder (storableTypeName (Just x) ^. unpacked) db (storableName x) x

  default load :: (FromJSON a) => DB -> Text -> IO (Either LoadError a)
  load = loadJSONFrom (storableTypeName @a undefined ^. unpacked)

  default find :: (FromJSON a) => DB -> Text -> Stream IO (Of (Either String a)) ()
  find = error "StorableDB.find unimplemented"


--------------------------------------------------------------------------------
--  'StorableDB' Instances
--------------------------------------------------------------------------------

instance StorableDB Ingredient where
  storableName = view name
  storableTypeName _ = "ingredients"

instance StorableDB Recipe where
  storableName = view title
  storableTypeName _ = "recipes"

--------------------------------------------------------------------------------
--  Internal
--------------------------------------------------------------------------------

saveJSONUnder :: (ToJSON a) => FilePath -> DB -> Text -> a -> IO ()
saveJSONUnder path db name' x = do
  let fullDir = (db ^. rootDir) </> path
      fullPath = fullDir </> nameToFilename name'
      dat = Aeson.encode x

  createDirectoryIfMissing True fullDir
  BS.writeFile fullPath dat

loadJSONFrom :: (FromJSON a) => FilePath -> DB -> Text -> IO (Either LoadError a)
loadJSONFrom path db name' = runExceptT $ do
  let fullDir = (db ^. rootDir) </> path
      fullPath = fullDir </> nameToFilename name'

  exists <- liftIO $ foldlM (\x -> fmap (x &&)) True
    [ doesDirectoryExist fullDir
    , doesFileExist fullPath
    ]
  unless exists $ throwError NoSuchObject

  dat <- liftIO $ BS.readFile fullPath

  either (throwError . MalformedData) return $
    Aeson.eitherDecode dat



-- TODO: Improve this
nameToFilename :: Text -> FilePath
nameToFilename = view unpacked

-- dbFilePath :: DBName -> IO FilePath
-- dbFilePath (DBName x)
