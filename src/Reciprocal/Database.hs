{-# LANGUAGE TemplateHaskell            #-}

module Reciprocal.Database
  (
    -- * Types
    Database
  , LoadError(..)
  , Handler(..)
  , Key

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

import           Control.Monad.Except        (ExceptT(..), runExceptT, throwError)
import           Data.Foldable               (foldlM)

import qualified Data.Conduit.Combinators as Conduit
import qualified Streaming.Prelude as S


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
  , rootPath  :: FilePath
  , store     :: a -> m ()
  , load      :: Key a -> m (Either LoadError a)
  , find      :: Text -> Stream (Of a) m ()
  }

--------------------------------------------------------------------------------
--  Generic Handlers
--------------------------------------------------------------------------------

getJsonHandler
    :: (FromJSON a, ToJSON a, MonadResource m)
    => Text -> (a -> Text) -> Database -> Handler m a
getJsonHandler typeName objectName db =
  let rootPath = (db ^. rootDir) </> (typeName ^. unpacked)
      objectKey = Key . view packed . nameToFilename . objectName
  in Handler
     { objectKey
     , rootPath
     , store = \x -> liftIO $ saveJSONUnder rootPath (objectKey x) x
     , load = liftIO . loadJSONFrom rootPath
     , find = findByTitle rootPath objectName
     }

--------------------------------------------------------------------------------
--  Specific handlers
--------------------------------------------------------------------------------

getIngredientHandler :: (MonadResource m) => Database -> Handler m Ingredient
getIngredientHandler = getJsonHandler "ingredients" (view name)

getRecipeHandler :: (MonadResource m) => Database -> Handler m Recipe
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

  ExceptT $ loadJSONSimple fullPath

loadJSONSimple :: (FromJSON a) => FilePath -> IO (Either LoadError a)
loadJSONSimple path = runExceptT $ do
  dat <- liftIO $ BS.readFile path

  either (throwError . MalformedData . view packed) return $
    Aeson.eitherDecode dat


-- TODO: search by something better than exact match
findByTitle
  :: forall m a. (FromJSON a, MonadResource m)
  => FilePath -> (a -> Text)
  -> Text -> Stream (Of a) m ()
findByTitle path objectTitle searchTerm =
  let files = conduitToStream $ Conduit.sourceDirectory path

      testFile :: FilePath -> m (Maybe a)
      testFile fp = do
        eobj <- liftIO $ loadJSONSimple fp
        case eobj of
          Left _ -> return Nothing
          Right x -> do
            let tit = objectTitle x
            return $ if tit == searchTerm
                     then Just x
                     else Nothing

  in S.mapMaybeM testFile files


-- TODO: Improve this
nameToFilename :: Text -> FilePath
nameToFilename = view unpacked
