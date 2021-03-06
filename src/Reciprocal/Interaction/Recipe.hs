module Reciprocal.Interaction.Recipe where

import Reciprocal.Prelude

import Reciprocal.Interaction.Core
import Reciprocal.Model.Recipe



viewRecipe :: (Monad m) => Recipe -> Interaction m RecipeView
viewRecipe r = return $ RecipeView
  { recipe = r
  , isEditing = False
  }
