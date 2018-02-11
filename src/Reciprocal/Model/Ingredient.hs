{-# LANGUAGE TemplateHaskell #-}

module Reciprocal.Model.Ingredient where

import           Reciprocal.Prelude

import           Reciprocal.Model.Measure

--------------------------------------------------------------------------------
--  Types
--------------------------------------------------------------------------------

data Ingredient =
  Ingredient
  { _ingredientName            :: Text
  , _ingredientDefaultUnitType :: UnitType
  , _ingredientCategory        :: Maybe Category
  }
  deriving (Eq, Ord, Show, Typeable, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype Category = UserCategory Text
  deriving (Eq, Ord, Show, Typeable, Generic)
  deriving anyclass (ToJSON, FromJSON)

--------------------------------------------------------------------------------
--  Lenses
--------------------------------------------------------------------------------

makeFields ''Ingredient
makePrisms ''Category
