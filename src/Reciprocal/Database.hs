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
  = MalformedData Text
  | NoSuchObject

newtype Key a = Key Text

data Handler m a = Handler
  { objectKey :: a -> Key a
  , rootPath :: FilePath
  , store :: a -> m ()
  , load :: Key a -> m (Either LoadError a)
  , find :: Text -> Stream m (Of (Either Text a)) ()
  }

--------------------------------------------------------------------------------
--  Generic Handlers
--------------------------------------------------------------------------------

getJsonHandler
    :: (FromJSON a, ToJSON a)
    => Text -> (a -> Text) -> Database -> Handler IO a
getJsonHandler typeName objectName db =
  let rootPath = (db ^. rootDir) </> (typeName ^. unpacked)
      objectKey = Key . view packed . nameToFilename . objectName
  in Handler
     { objectKey
     , rootPath
     , store = \x -> saveJSONUnder rootPath (objectKey x) x
     , load = loadJSONFrom rootPath
     , find = error $
       "Handler.find unimplemented for " <> (display typeName ^. unpacked)
     }

--------------------------------------------------------------------------------
--  Specific handlers
--------------------------------------------------------------------------------

getIngredientHandler :: Database -> Handler IO Ingredient
getIngredientHandler = getJsonHandler "ingredients" (view name)

getRecipeHandler :: Database -> Handler IO Recipe
getRecipeHandler = getJsonHandler "recipes" (view title)

--------------------------------------------------------------------------------
--  Internal
--------------------------------------------------------------------------------

saveJSONUnder :: (ToJSON a) => FilePath -> Key a -> a -> IO ()
saveJSONUnder path (Key fname) x = do
  let fullPath = path </> (fname ^. unpacked)
      dat = Aeson.encode x

  createDirectoryIfMissing True path
  BS.writeFile fullPath dat

loadJSONFrom :: (FromJSON a) => FilePath -> Key a -> IO (Either LoadError a)
loadJSONFrom path (Key fname) = runExceptT $ do
  let fullPath = path </> nameToFilename fname

  exists <- liftIO $ foldlM (\x -> fmap (x &&)) True
    [ doesDirectoryExist path
    , doesFileExist fullPath
    ]
  unless exists $ throwError NoSuchObject

  dat <- liftIO $ BS.readFile fullPath

  either (throwError . MalformedData . view packed) return $
    Aeson.eitherDecode dat



-- TODO: Improve this
nameToFilename :: Text -> FilePath
nameToFilename = view unpacked
