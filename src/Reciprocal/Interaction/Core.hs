{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Reciprocal.Interaction.Core where

import Reciprocal.Prelude

import Reciprocal.Database
import Reciprocal.Model.Recipe
import Reciprocal.Model.Ingredient
import qualified Reciprocal.Logging as L

--------------------------------------------------------------------------------
--  Main Types
--------------------------------------------------------------------------------

data Env m = Env
  { _envRecipeHandler :: Handler m Recipe
  , _envIngredientHandler :: Handler m Ingredient
  , _envLogger :: L.Logger m
  }

newtype Interaction m a = Interaction
  { getInteraction :: ReaderT (Env m) m a
  }
  deriving newtype
  ( Functor
  , Applicative
  , Monad
  , MonadReader (Env m)
  )

instance MonadTrans Interaction where
  lift = Interaction . lift

--------------------------------------------------------------------------------
--  Views
--------------------------------------------------------------------------------

data SomeView
  = SVBrowse BrowseView
  | SVRecipe RecipeView
  | SVMealPlan MealPlanView
  | SVExportShopping ExportShoppingView
  | SVListShopping ListShoppingView
  deriving (Show)


data BrowseView = BrowseView
  { _browseViewRecipes :: [Recipe]
  , _browseViewSearchTerm :: Text
  }
  deriving (Show)

data RecipeView = RecipeView
  { _recipeViewRecipe :: Recipe
  , _recipeViewIsEditing :: Bool
  }
  deriving (Show)

data MealPlanView = MealPlanView
  deriving (Show)

data ExportShoppingView = ExportShoppingView
  deriving (Show)

data ListShoppingView = ListShoppingView
  deriving (Show)

--------------------------------------------------------------------------------
--  Lenses
--------------------------------------------------------------------------------

makeFields ''Env

makePrisms ''SomeView

makeFields ''BrowseView
makeFields ''RecipeView
makeFields ''MealPlanView
makeFields ''ExportShoppingView
makeFields ''ListShoppingView

--------------------------------------------------------------------------------
--  Combinators
--------------------------------------------------------------------------------

logWarning :: (Monad m) => Text -> Interaction m ()
logWarning msg = do
  lg <- view logger
  lift $ L.logWarning lg msg
