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

import qualified Streaming.Prelude as S


data Env m = Env
  { ingredientHandler :: Handler m Ingredient
  , recipeHandler :: Handler m Recipe
  , logger :: L.Logger m
  }
  deriving (Generic)

newtype Resolve m a = Resolve { getResolve :: ReaderT (Env m) m a }
  deriving newtype (Functor, Applicative, Monad)

instance MonadTrans Resolve where
  lift = Resolve . lift

runResolveIO :: Resolve (ResourceT IO) a -> Config -> IO a
runResolveIO action cfg = do
  db <- openDB cfg
  runResourceT $ runReaderT
    (getResolve action) $ Env
    { ingredientHandler = getIngredientHandler db
    , recipeHandler = getRecipeHandler db
    , logger =L.stdioLogger
    }


resolveIngredient :: (Monad m) => Text -> Resolve m (Maybe Ingredient)
resolveIngredient nm = runMaybeT $ do
  ih <- lift . Resolve $ view (field @"ingredientHandler")
  findRes <- lift . lift $ S.next (find ih nm)

  case findRes of
    Left () -> createIngredient nm
    Right (x, _) -> return x


-- TODO: Actually create ingredient
createIngredient :: (Monad m) => Text -> MaybeT (Resolve m) Ingredient
createIngredient nm = do
  lift . logWarning $ "ingredient doesn't exist in the database: " <> display nm
  empty


resolveRecipe :: (Monad m) => Text -> Resolve m (Maybe Recipe)
resolveRecipe nm = runMaybeT $ do
  rh <- lift . Resolve $ view (field @"recipeHandler")
  findRes <- lift . lift $ S.next (find rh nm)

  case findRes of
    Left () -> createRecipe nm
    Right (x, _) -> return x


-- TODO: Actually create recipe
createRecipe :: (Monad m) => Text -> MaybeT (Resolve m) Recipe
createRecipe nm = do
  lift . logWarning $ "recipe doesn't exist in the database: " <> display nm
  empty

logWarning :: (Monad m) => Text -> Resolve m ()
logWarning msg = do
  lg <- Resolve (view (field @"logger"))
  lift $ L.logWarning lg msg
