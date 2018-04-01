module Reciprocal.Model.Recipe where

import           Reciprocal.Prelude

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
  deriving (Eq, Ord, Show, Typeable, Generic)
  deriving anyclass (ToJSON, FromJSON)

data IngredientAttribute
  = Chopped IngredientFineness
  | Sliced IngredientFineness
  | Diced IngredientFineness
  | Minced IngredientFineness
  | OtherAttr Text
  deriving (Eq, Ord, Show, Typeable, Generic)
  deriving anyclass (ToJSON, FromJSON)

data RecipeIngredient =
  RecipeIngredient
  { ingredient :: Either Text Ingredient
    -- ^ 'Text' if the ingredient is unlisted in the database
  , measure :: Some MeasureRange
  , attributes  :: [IngredientAttribute]
  }
  deriving (Show, Typeable, Generic)
  deriving anyclass (ToJSON, FromJSON)

data RecipeDuration
  = ActiveTotal Duration Duration
  | Undivided Duration
  deriving (Show, Typeable, Generic)
  deriving anyclass (ToJSON, FromJSON)

data RecipeSource
  = SourceWebsite URI
  | SourceOther Text
  deriving (Eq, Ord, Show, Typeable, Generic)
  deriving anyclass (ToJSON, FromJSON)

data Recipe =
  Recipe
  { title        :: Text
  , description  :: Maybe Text
  , source       :: Maybe RecipeSource
  , duration     :: Maybe RecipeDuration
  , servings     :: Maybe Rational
  , ingredients  :: [RecipeIngredient]
  , instructions :: [Text]
  }
  deriving (Show, Typeable, Generic)
  deriving anyclass (ToJSON, FromJSON)

--------------------------------------------------------------------------------
--  Instances
--------------------------------------------------------------------------------

instance Semigroup Recipe where
  r1 <> r2 =
    Recipe
    { title        = (r1 ^. field @"title") <> (r2 ^. field @"title")
    , description  = (r1 ^. field @"description") <|> (r2 ^. field @"description")
    , source       = (r1 ^. field @"source") <|> (r2 ^. field @"source")
    , duration     = (r1 ^. field @"duration") <|> (r2 ^. field @"duration")
    , servings     = (r1 ^. field @"servings") <|> (r2 ^. field @"servings")
    , ingredients  = (r1 ^. field @"ingredients") <> (r2 ^. field @"ingredients")
    , instructions = (r1 ^. field @"instructions") <> (r2 ^. field @"instructions")
    }

instance Monoid Recipe where
  mempty = Recipe mempty Nothing Nothing Nothing Nothing [] []
  mappend = (<>)
