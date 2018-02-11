{-# LANGUAGE TemplateHaskell #-}

{-|
Resolving parts of recipes using items in the database.
-}
module Reciprocal.Resolution.Recipe
  ( Resolve
  , runResolve
  , resolveIngredient
  , resolveRecipe
  ) where

import Reciprocal.Prelude

import Reciprocal.Model.Ingredient
import Reciprocal.Model.Recipe

import Reciprocal.Config
import Reciprocal.Database

import Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import Control.Monad.Reader (ReaderT, runReaderT)


data Env = Env
  { _envIngredientHandler :: Handler Ingredient
  , _envRecipeHandler :: Handler Recipe
  }

makeFields ''Env

newtype Resolve a = Resolve { getResolve :: ReaderT Env IO a }
  deriving newtype (Functor, Applicative, Monad, MonadIO)


runResolve :: Resolve a -> Config -> IO a
runResolve action cfg = do
  db <- openDB cfg
  runReaderT (getResolve action) (Env (getIngredientHandler db) (getRecipeHandler db))


resolveIngredient :: Text -> Resolve (Maybe Ingredient)
resolveIngredient nm = runMaybeT $ do
  ih <- lift . Resolve $ view ingredientHandler
  fromDb <- liftIO (load ih nm)

  case fromDb of
    Left (MalformedData msg) -> fixMalformedIngredient msg nm
    Left NoSuchObject -> createIngredient nm
    Right x -> return x


-- TODO: Let user fix it properly
fixMalformedIngredient :: String -> Text -> MaybeT Resolve Ingredient
fixMalformedIngredient msg nm = do
  liftIO $ putStrLn $ "malformed ingredient in database: " <> show nm
  liftIO $ putStrLn msg
  empty


-- TODO: Actually create ingredient
createIngredient :: Text -> MaybeT Resolve Ingredient
createIngredient nm = do
  liftIO $ putStrLn $ "ingredient doesn't exist in the database: " <> show nm
  empty


resolveRecipe :: Text -> Resolve (Maybe Recipe)
resolveRecipe nm = runMaybeT $ do
  rh <- lift . Resolve $ view recipeHandler
  fromDb <- liftIO (load rh nm)

  case fromDb of
    Left (MalformedData msg) -> fixMalformedRecipe msg nm
    Left NoSuchObject -> createRecipe nm
    Right x -> return x


-- TODO: Let user fix it properly
fixMalformedRecipe :: String -> Text -> MaybeT Resolve Recipe
fixMalformedRecipe msg nm = do
  liftIO $ putStrLn $ "malformed recipe in database: " <> show nm
  liftIO $ putStrLn msg
  empty


-- TODO: Actually create recipe
createRecipe :: Text -> MaybeT Resolve Recipe
createRecipe nm = do
  liftIO $ putStrLn $ "recipe doesn't exist in the database: " <> show nm
  empty
