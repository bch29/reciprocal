{-# LANGUAGE TemplateHaskell #-}

{-|
Resolving parts of recipes using items in the database.
-}
module Reciprocal.Resolution.Recipe
  ( Resolve
  , runResolveIO
  , resolveIngredient
  , resolveRecipe
  ) where

import Reciprocal.Prelude

import Reciprocal.Model.Ingredient
import Reciprocal.Model.Recipe

import Reciprocal.Config
import Reciprocal.Database
import qualified Reciprocal.Logging as L


data Env m = Env
  { _envIngredientHandler :: Handler m Ingredient
  , _envRecipeHandler :: Handler m Recipe
  , _envLogger :: L.Logger m
  }

makeFields ''Env

newtype Resolve m a = Resolve { getResolve :: ReaderT (Env m) m a }
  deriving newtype (Functor, Applicative, Monad)

instance MonadTrans Resolve where
  lift = Resolve . lift

runResolveIO :: Resolve IO a -> Config -> IO a
runResolveIO action cfg = do
  db <- openDB cfg
  runReaderT (getResolve action) (Env (getIngredientHandler db) (getRecipeHandler db) L.stdioLogger)


resolveIngredient :: (Monad m) => Text -> Resolve m (Maybe Ingredient)
resolveIngredient nm = runMaybeT $ do
  ih <- lift . Resolve $ view ingredientHandler
  fromDb <- lift . lift $ load ih nm

  case fromDb of
    Left (MalformedData msg) -> fixMalformedIngredient msg nm
    Left NoSuchObject -> createIngredient nm
    Right x -> return x


-- TODO: Let user fix it properly
fixMalformedIngredient :: (Monad m) => Text -> Text -> MaybeT (Resolve m) Ingredient
fixMalformedIngredient msg nm = do
  lift $ do
    logWarning $ "malformed ingredient in database: " <> display nm
    logWarning msg
  empty


-- TODO: Actually create ingredient
createIngredient :: (Monad m) => Text -> MaybeT (Resolve m) Ingredient
createIngredient nm = do
  lift . logWarning $ "ingredient doesn't exist in the database: " <> display nm
  empty


resolveRecipe :: (Monad m) => Text -> Resolve m (Maybe Recipe)
resolveRecipe nm = runMaybeT $ do
  rh <- lift . Resolve $ view recipeHandler
  fromDb <- lift . lift $ load rh nm

  case fromDb of
    Left (MalformedData msg) -> fixMalformedRecipe msg nm
    Left NoSuchObject -> createRecipe nm
    Right x -> return x


-- TODO: Let user fix it properly
fixMalformedRecipe :: (Monad m) => String -> Text -> MaybeT (Resolve m) Recipe
fixMalformedRecipe msg nm = do
  lift $ do
    logWarning $ "malformed recipe in database: " <> display nm
    logWarning msg
  empty


-- TODO: Actually create recipe
createRecipe :: (Monad m) => Text -> MaybeT (Resolve m) Recipe
createRecipe nm = do
  lift . logWarning $ "recipe doesn't exist in the database: " <> display nm
  empty

logWarning :: (Monad m) => Text -> Resolve m ()
logWarning msg = do
  logger <- Resolve (view logger)
  lift $ L.logWarning logger msg
