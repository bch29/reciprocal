{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}

module Reciprocal.Database
  (
    -- * Types
    Database
  , LoadError(..)
  , Handler(..)

    -- * Opening a 'Database'
  , openDB

    -- * Handlers
  , getIngredientHandler
  , getRecipeHandler
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

data Database = Database
  { _databaseRootDir :: FilePath
  }

makeFields ''Database

-- | Opens the database, and creates it if it doesn't exist. Throws an exception
-- if some file system error was encountered while opening the database.
openDB :: Config -> IO Database
openDB cfg = do
  let _databaseRootDir = cfg ^. rootDir
  createDirectoryIfMissing True _databaseRootDir
  return (Database { _databaseRootDir })

data LoadError
  = MalformedData String
  | NoSuchObject

data Handler a = Handler
  { objectName :: a -> Text
  , rootPath :: FilePath
  , store :: a -> IO ()
  , load :: Text -> IO (Either LoadError a)
  , find :: Text -> Stream IO (Of (Either String a)) ()
  }

--------------------------------------------------------------------------------
--  Generic Handlers
--------------------------------------------------------------------------------

getJsonHandler :: (FromJSON a, ToJSON a) => Text -> (a -> Text) -> Database -> Handler a
getJsonHandler typeName objectName db =
  let rootPath = (db ^. rootDir) </> (typeName ^. unpacked)
  in Handler
     { objectName
     , rootPath
     , store = \x -> saveJSONUnder rootPath (objectName x) x
     , load = loadJSONFrom rootPath
     , find = error "Handler.find unimplemented"
     }

--------------------------------------------------------------------------------
--  Specific handlers
--------------------------------------------------------------------------------

getIngredientHandler :: Database -> Handler Ingredient
getIngredientHandler = getJsonHandler "ingredients" (view name)

getRecipeHandler :: Database -> Handler Recipe
getRecipeHandler = getJsonHandler "recipes" (view title)

--------------------------------------------------------------------------------
--  Internal
--------------------------------------------------------------------------------

saveJSONUnder :: (ToJSON a) => FilePath -> Text -> a -> IO ()
saveJSONUnder path name' x = do
  let fullPath = path </> nameToFilename name'
      dat = Aeson.encode x

  createDirectoryIfMissing True path
  BS.writeFile fullPath dat

loadJSONFrom :: (FromJSON a) => FilePath -> Text -> IO (Either LoadError a)
loadJSONFrom path name' = runExceptT $ do
  let fullPath = path </> nameToFilename name'

  exists <- liftIO $ foldlM (\x -> fmap (x &&)) True
    [ doesDirectoryExist path
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
