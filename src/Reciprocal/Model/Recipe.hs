{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}

module Reciprocal.Model.Recipe where

import           Reciprocal.Prelude

import qualified URI.ByteString              as URI

import           Reciprocal.Model.Duration
import           Reciprocal.Model.Ingredient
import           Reciprocal.Model.Measure

--------------------------------------------------------------------------------
--  Types
--------------------------------------------------------------------------------

data IngredientFineness
  = VeryFine
  | Fine
  | MediumFine
  | Rough
  | VeryRough
  deriving (Eq, Ord, Show)

data IngredientAttribute
  = Chopped IngredientFineness
  | Sliced IngredientFineness
  | Diced IngredientFineness
  | OtherAttr Text

data RecipeIngredient =
  RecipeIngredient
  { _recipeIngredientIngredient :: Either Text Ingredient
    -- ^ 'Text' if the ingredient is unlisted in the database
  , _recipeIngredientMeasure    :: Some Measure
  , _recipeIngredientAttributes :: [IngredientAttribute]
  }

data RecipeDuration
  = ActiveTotal Duration Duration
  | Undivided Duration

data RecipeSource
  = SourceWebsite (URI.URIRef URI.Absolute)
  | SourceOther Text
  deriving (Eq, Ord)

data Recipe =
  Recipe
  { _recipeTitle        :: Text
  , _recipeDescription  :: Maybe Text
  , _recipeSource       :: Maybe RecipeSource
  , _recipeDuration     :: Maybe RecipeDuration
  , _recipeIngredients  :: [RecipeIngredient]
  , _recipeInstructions :: [Text]
  }

--------------------------------------------------------------------------------
--  Lenses
--------------------------------------------------------------------------------

makePrisms ''IngredientFineness
makePrisms ''IngredientAttribute
makeFields ''RecipeIngredient
makeLenses ''RecipeDuration
makePrisms ''RecipeSource
makeFields ''Recipe

--------------------------------------------------------------------------------
--  Instances
--------------------------------------------------------------------------------

instance Semigroup Recipe where
  r1 <> r2 =
    Recipe
    ((r1 ^. title) <> (r2 ^. title))
    ((r1 ^. description) <|> (r2 ^. description))
    ((r1 ^. source) <|> (r2 ^. source))
    ((r1 ^. duration) <|> (r2 ^. duration))
    ((r1 ^. ingredients) <> (r2 ^. ingredients))
    ((r1 ^. instructions) <> (r2 ^. instructions))

instance Monoid Recipe where
  mempty = Recipe mempty Nothing Nothing Nothing [] []
  mappend = (<>)

-- data Recipe

