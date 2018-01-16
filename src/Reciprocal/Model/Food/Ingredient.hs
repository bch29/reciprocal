{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

module Reciprocal.Model.Food.Ingredient where

import Reciprocal.Prelude

import Reciprocal.Model.Food.Measure

--------------------------------------------------------------------------------
--  Types
--------------------------------------------------------------------------------

data Ingredient =
  Ingredient
  { _ingredientName :: Text
  , _ingredientDefaultUnitType :: UnitType
  , _ingredientCategory :: Maybe Category
  }

data Category = UserCategory Text

--------------------------------------------------------------------------------
--  Lenses
--------------------------------------------------------------------------------

makeFields ''Ingredient
