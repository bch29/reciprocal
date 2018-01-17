{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE TemplateHaskell        #-}

module Reciprocal.Model.Measure where

import           Reciprocal.Prelude

import           Data.Ratio         (denominator, numerator)

--------------------------------------------------------------------------------
--  Constants
--------------------------------------------------------------------------------

litresPerTeaspoon :: Rational
litresPerTeaspoon = 5 / 1000

litresPerTablespoon :: Rational
litresPerTablespoon = 15 / 1000

--------------------------------------------------------------------------------
--  Types
--------------------------------------------------------------------------------

data Measure t
  = Measure
  { _measureAmount :: Rational
  , _measureUnit :: Unit t
  }

data UnitType
  = Mass
  | Volume
  | WholeT
  | ClovesT

data Unit t where
  Grams :: Unit 'Mass
  Litres :: Unit 'Volume
  Teaspoons, Tablespoons :: Unit 'Volume

  Magnify :: Integer -> Unit t -> Unit t

  Whole :: Unit 'WholeT
  Cloves :: Unit 'ClovesT

--------------------------------------------------------------------------------
--  Instances
--------------------------------------------------------------------------------

deriving instance Eq (Unit t)

--------------------------------------------------------------------------------
--  Lenses
--------------------------------------------------------------------------------

makeFields ''Measure

--------------------------------------------------------------------------------
--  Smart Constructors
--------------------------------------------------------------------------------

magnifyUnit :: Integer -> Unit t -> Unit t
magnifyUnit x (Magnify y u) = magnifyUnit (x + y) u
magnifyUnit x u = Magnify x u

kilo :: Unit t -> Unit t
kilo = magnifyUnit 3

milli :: Unit t -> Unit t
milli = magnifyUnit (-3)

normaliseUnit :: Unit t -> Unit t
normaliseUnit = magnifyUnit 0

magnifyMeasureUnit :: Integer -> Measure t -> Measure t
magnifyMeasureUnit x =
  over unit (magnifyUnit x) .
  over amount (* (10 ^ (-x)))

-- | Given a unit, return the standard unit of the same 'UnitType', along with
-- the factor to multiply an amount of the old unit by to get an amount of the
-- new unit.
toStandardUnits :: Unit t -> (Unit t, Rational)
toStandardUnits (Magnify x u) =
  let (u', y) = toStandardUnits u
  in (u', y * 10^(-x))
toStandardUnits Teaspoons = (Litres, 1 / litresPerTeaspoon)
toStandardUnits Tablespoons = (Litres, 1 / litresPerTablespoon)
toStandardUnits other = (other, 1)

-- | Converts the measure into base units. Litres or grams, with no
-- magnification.
toStandardMeasure :: Measure t -> Measure t
toStandardMeasure m =
  let (newUnit, rescaleFactor) = toStandardUnits (m ^. unit)
  in Measure ((m ^. amount) * rescaleFactor) newUnit


-- | If the measure is in litres, and is a volume that is easier to
-- understand in terms of teaspoons or tablespoons, converts it to spoons.
-- Otherwise leaves it as is.
tryLitresToSpoons :: Measure t -> Measure t
tryLitresToSpoons m =
  case m ^. unit of
    Litres ->
      let conversions =
            [ (litresPerTeaspoon * n, Teaspoons) | n <- [1/8, 2/8 .. 2]] ++
            [ (litresPerTeaspoon * n, Tablespoons) | n <- [1/4, 2/4 .. 8]]
      in case lookup (m ^. amount) conversions of
        Just newUnit -> convertMeasureSimple newUnit m
        Nothing -> m
    _ -> m

-- | Converts the measure into units such that the amount is between 1 and
-- 1000.
remagnifyMeasure :: Measure t -> Measure t
remagnifyMeasure m = magnifyMeasureUnit (-logAmountThrees) m
  where
    logAmount = logBase 10 (fromRational (m ^. amount) :: Double)
    logAmountThrees = floor (logAmount / 3)

-- | Converts the measure into those units that are most understandable by
-- humans.
--
-- TODO: make this configurable, e.g. for choosing between metric and imperial.
toUserMeasure :: Measure t -> Measure t
toUserMeasure = remagnifyMeasure . tryLitresToSpoons . toStandardMeasure

--------------------------------------------------------------------------------
--  Conversion
--------------------------------------------------------------------------------

{- |
Returns how many times bigger the given unit is than the standard unit of
the same 'UnitType'. For the purposes of this library, the standard units are:

- 'Mass': 'Grams'
- 'Volume': 'Litres'
-}
multiplierFromStandard :: Unit t -> Rational
multiplierFromStandard Teaspoons = litresPerTeaspoon
multiplierFromStandard Tablespoons = litresPerTablespoon
multiplierFromStandard (Magnify x u) = (10^x) * multiplierFromStandard u
multiplierFromStandard _ = 1

multiplierFromTo :: Unit t -> Unit t -> Rational
multiplierFromTo fromUnit toUnit =
  multiplierFromStandard toUnit / multiplierFromStandard fromUnit

-- | Converts a measure to a different unit of the same type. This
-- restriction means that no other information (such as density) is required.
convertMeasureSimple :: Unit t -> Measure t -> Measure t
convertMeasureSimple toUnit measure =
  Measure
    ((measure ^. amount) * fromRational (multiplierFromTo (measure ^. unit) toUnit))
    toUnit

--------------------------------------------------------------------------------
--  Pretty Printing
--------------------------------------------------------------------------------

prettyUnit :: Unit t -> String
prettyUnit = prettyUnit' . normaliseUnit
  where
  prettyUnit' Grams = "g"
  prettyUnit' Litres = "l"
  prettyUnit' Teaspoons = "tsp"
  prettyUnit' Tablespoons = "tbsp"
  prettyUnit' (Magnify 3 u) = "k" ++ prettyUnit' u
  prettyUnit' (Magnify 6 u) = "g" ++ prettyUnit' u
  prettyUnit' (Magnify (-3) u) = "m" ++ prettyUnit' u
  prettyUnit' (Magnify (-6) u) = "µ" ++ prettyUnit' u
  prettyUnit' (Magnify x u) = prettyUnit' u ++ "×10^" ++ show x
  prettyUnit' Cloves = "cloves"
  prettyUnit' Whole = "whole"

prettyMeasure :: Measure t -> String
prettyMeasure (Measure x u) =
  prettyAmount ++ " " ++ prettyUnit u
  where prettyAmount =
          if denominator x == 1
          then show (numerator x)
          else show (fromRational x :: Double)
