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
  { recipeHandler :: Handler m Recipe
  , ingredientHandler :: Handler m Ingredient
  , logger :: L.Logger m
  }
  deriving (Generic)

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
  deriving (Show, Generic)


data BrowseView = BrowseView
  { recipes :: [Recipe]
  , searchTerm :: Text
  }
  deriving (Show, Generic)

data RecipeView = RecipeView
  { recipe :: Recipe
  , isEditing :: Bool
  }
  deriving (Show, Generic)

data MealPlanView = MealPlanView
  deriving (Show, Generic)

data ExportShoppingView = ExportShoppingView
  deriving (Show, Generic)

data ListShoppingView = ListShoppingView
  deriving (Show, Generic)

--------------------------------------------------------------------------------
--  Combinators
--------------------------------------------------------------------------------

logWarning :: (Monad m) => Text -> Interaction m ()
logWarning msg = do
  lg <- view (field @"logger")
  lift $ L.logWarning lg msg
