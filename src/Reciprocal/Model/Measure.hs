{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}

module Reciprocal.Model.Measure where

import           Reciprocal.Prelude

import           Data.Aeson
import           Data.Aeson.Types   (typeMismatch)
import           Data.Ratio         (denominator, numerator)
import           Type.Class.Higher  (Show1 (..))

import           Text.Show

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

data MeasureRange t
  = SingleMR (Measure t)
  | RangeMR (Measure t) (Measure t)
  deriving (Generic, Typeable, Eq, Show)
  deriving anyclass (ToJSON)

data Measure (t :: UnitType)
  = Measure
  { _measureAmount :: Rational
  , _measureUnit   :: Unit t
  }
  deriving (Generic, Typeable, Eq, Show)
  deriving anyclass (ToJSON)

data UnitType
  = Mass
  | Volume
  | WholeT
  | ClovesT
  deriving (Generic, Typeable, Eq, Ord, Show)
  deriving anyclass (FromJSON, ToJSON)

data MassUnit = MUGrams
  deriving (Generic, Typeable, Eq, Ord, Show)
  deriving anyclass (FromJSON, ToJSON)
data VolumeUnit = VULitres | VUTeaspoons | VUTablespoons
  deriving (Generic, Typeable, Eq, Ord, Show)
  deriving anyclass (FromJSON, ToJSON)
data WholeUnit = WUWhole
  deriving (Generic, Typeable, Eq, Ord, Show)
  deriving anyclass (FromJSON, ToJSON)
data ClovesUnit = CUCloves
  deriving (Generic, Typeable, Eq, Ord, Show)
  deriving anyclass (FromJSON, ToJSON)

data Unit t where
  MU :: MassUnit -> Unit 'Mass
  VU :: VolumeUnit -> Unit 'Volume
  WU :: WholeUnit -> Unit 'WholeT
  CU :: ClovesUnit -> Unit 'ClovesT
  Magnify :: Integer -> Unit t -> Unit t
  deriving (Typeable)

pattern Grams :: forall t. () => t ~ 'Mass => Unit t
pattern Grams = MU MUGrams

pattern Litres :: forall t. () => t ~ 'Volume => Unit t
pattern Litres = VU VULitres

pattern Teaspoons :: forall t. () => t ~ 'Volume => Unit t
pattern Teaspoons = VU VUTeaspoons

pattern Tablespoons :: forall t. () => t ~ 'Volume => Unit t
pattern Tablespoons = VU VUTablespoons

pattern Whole :: forall t. () => t ~ 'WholeT => Unit t
pattern Whole = WU WUWhole

pattern Cloves :: forall t. () => t ~ 'ClovesT => Unit t
pattern Cloves = CU CUCloves


--------------------------------------------------------------------------------
--  Instances
--------------------------------------------------------------------------------

instance Show1 MeasureRange where showsPrec1 = showsPrec

instance Show1 Measure where showsPrec1 = showsPrec

deriving instance Typeable (Unit t)
deriving instance Eq (Unit t)
-- deriving instance Ord (Unit t)
deriving instance Show (Unit t)
instance Show1 Unit where showsPrec1 = showsPrec

data instance Sing (t :: UnitType) where
  SMass :: Sing 'Mass
  SVolume :: Sing 'Volume
  SWholeT :: Sing 'WholeT
  SClovesT :: Sing 'ClovesT

instance SingI 'Mass where sing = SMass
instance SingI 'Volume where sing = SVolume
instance SingI 'WholeT where sing = SWholeT
instance SingI 'ClovesT where sing = SClovesT

instance ToJSON (Unit t) where
  toJSON = \case
    MU u -> Object (mempty & at "type" .~ Just "mass" & at "unit" .~ Just (toJSON u))
    VU u -> Object (mempty & at "type" .~ Just "volume" & at "unit" .~ Just (toJSON u))
    WU u -> Object (mempty & at "type" .~ Just "whole" & at "unit" .~ Just (toJSON u))
    CU u -> Object (mempty & at "type" .~ Just "cloves" & at "unit" .~ Just (toJSON u))
    Magnify x u -> Object (mempty & at "type" .~ Just "magnify"
                                  & at "scale" .~ Just (toJSON x)
                                  & at "unit" .~ Just (toJSON u))

instance ToJSON (Some Unit) where
  toJSON (Some u) = toJSON u
instance ToJSON (Some Measure) where
  toJSON (Some m) = toJSON m
instance ToJSON (Some MeasureRange) where
  toJSON (Some r) = toJSON r

instance FromJSON (Some Unit) where
  parseJSON (Object v) = do
    ty :: Text <- v .: "type"
    case ty of
      "mass" -> Some . MU <$> v .: "unit"
      "volume" -> Some . VU <$> v .: "unit"
      "whole" -> Some . WU <$> v .: "unit"
      "cloves" -> Some . CU <$> v .: "unit"
      "magnify" -> do
        Some u <- v .: "unit"
        fmap Some $ Magnify <$> v .: "scale" <*> pure u
      other -> fail $ "Unrecognised unit type " <> show other
  parseJSON invalid = typeMismatch "Some Unit" invalid

instance FromJSON (Some Measure) where
  parseJSON = withObject "Some Measure" $ \v ->
    do Some u <- v .: "_measureUnit"
       fmap Some $ Measure <$> v .: "_measureAmount" <*> pure u

instance FromJSON (Some MeasureRange) where
  parseJSON = withObject "Some MeasureRange" $ \v ->
    do tag :: Text <- v .: "tag"
       case tag of
         "SingleMR" -> do
           Some m <- v .: "contents"
           return $ Some $ SingleMR m
         "RangeMR" -> do
           arr :: Array <- v .: "contents"
           v1 : v2 : [] <- return (arr ^.. traversed)
           Some m1 <- parseJSON v1
           Some m2 <- parseJSON v2
           case gcast m2 of
             Just m2' -> return (Some (RangeMR m1 m2'))
             Nothing -> fail "Range of measures of different unit types"
         other -> fail $ "Unrecognised MeasureRange tag " <> show other

--------------------------------------------------------------------------------
--  Lenses
--------------------------------------------------------------------------------

makeFields ''Measure
makePrisms ''MeasureRange

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

--------------------------------------------------------------------------------
--  Combinators
--------------------------------------------------------------------------------

infixl 5 `addMeasure`

addMeasure :: Measure t -> Measure t -> Measure t
addMeasure m1 m2 =
  let Measure a1 u = toStandardMeasure m1
      Measure a2 _ = toStandardMeasure m2
  in Measure (a1 + a2) u

--------------------------------------------------------------------------------
--  Conversion
--------------------------------------------------------------------------------

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

prettyMassUnit :: MassUnit -> Text
prettyMassUnit MUGrams = "g"

prettyVolumeUnit :: VolumeUnit -> Text
prettyVolumeUnit VULitres = "l"
prettyVolumeUnit VUTeaspoons = "tsp"
prettyVolumeUnit VUTablespoons = "tbsp"

prettyWholeUnit :: WholeUnit -> Text
prettyWholeUnit WUWhole = "whole"

prettyClovesUnit :: ClovesUnit -> Text
prettyClovesUnit CUCloves = "cloves"

prettyUnit :: Unit t -> Text
prettyUnit = prettyUnit' . normaliseUnit
  where
  prettyUnit' (MU u) = prettyMassUnit u
  prettyUnit' (VU u) = prettyVolumeUnit u
  prettyUnit' (WU u) = prettyWholeUnit u
  prettyUnit' (CU u) = prettyClovesUnit u
  prettyUnit' (Magnify 3 u) = "k" <> prettyUnit' u
  prettyUnit' (Magnify 6 u) = "g" <> prettyUnit' u
  prettyUnit' (Magnify (-3) u) = "m" <> prettyUnit' u
  prettyUnit' (Magnify (-6) u) = "µ" <> prettyUnit' u
  prettyUnit' (Magnify x u) = prettyUnit' u <> "×10^" <> display x

prettyMeasure :: Measure t -> Text
prettyMeasure (Measure x u) = prettyAmount <> " " <> prettyUnit u
  where
    prettyAmount =
      if denominator x == 1
      then display (numerator x)
      else display (fromRational x :: Double)
