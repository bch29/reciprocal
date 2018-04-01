module Reciprocal.Model.Ingredient where

import           Reciprocal.Prelude

import           Reciprocal.Model.Measure

--------------------------------------------------------------------------------
--  Types
--------------------------------------------------------------------------------

data Ingredient =
  Ingredient
  { name            :: Text
  , defaultUnitType :: UnitType
  , category        :: Maybe Category
  }
  deriving (Eq, Ord, Show, Typeable, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype Category = UserCategory Text
  deriving (Eq, Ord, Show, Typeable, Generic)
  deriving anyclass (ToJSON, FromJSON)
