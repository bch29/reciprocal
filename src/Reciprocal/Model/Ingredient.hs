{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE TemplateHaskell        #-}

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
  deriving (Eq, Ord, Show)

data Category = UserCategory Text
  deriving (Eq, Ord, Show)

--------------------------------------------------------------------------------
--  Lenses
--------------------------------------------------------------------------------

makeFields ''Ingredient
