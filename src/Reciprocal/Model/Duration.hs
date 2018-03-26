module Reciprocal.Model.Duration
  ( Duration
  , _Hours
  , _Minutes
  , _Seconds
  , addDuration
  , prettyDuration
  ) where

import           Reciprocal.Prelude

import           Text.Show          (showsPrec, showParen, showString)

--------------------------------------------------------------------------------
--  Types
--------------------------------------------------------------------------------

data Duration = Hours Double
  deriving (Eq, Ord, Typeable, Generic)
  deriving anyclass (FromJSON, ToJSON)

--------------------------------------------------------------------------------
--  Exported Lenses
--------------------------------------------------------------------------------

_Hours :: Iso' Duration Double
_Hours = iso durationHours Hours

_Minutes :: Iso' Duration Double
_Minutes = _Hours . iso (* 60) (/ 60)

_Seconds :: Iso' Duration Double
_Seconds = _Minutes . iso (* 60) (/ 60)

--------------------------------------------------------------------------------
--  Instances
--------------------------------------------------------------------------------

instance Show Duration where
  showsPrec p (Hours x) =
    showString "_Hours # " . showParen (p > 8) (showsPrec 9 x)

--------------------------------------------------------------------------------
--  Exported Other
--------------------------------------------------------------------------------

infixl 6 `addDuration`

addDuration :: Duration -> Duration -> Duration
addDuration (Hours x) (Hours y) = Hours (x + y)

prettyDuration :: Duration -> Text
prettyDuration d =
  let wholeHours = floor (d^._Hours) :: Int
      wholeMinutes = floor (d^._Minutes) :: Int
      wholeSeconds = floor (d^._Seconds) :: Int
      fracMinutes = wholeMinutes - wholeHours * 60
      fracSeconds = wholeSeconds - wholeMinutes * 60
  in if fracSeconds == 0
     then if fracMinutes == 0
          then display wholeHours <> " h"
          else display wholeHours <> " h " <> display fracMinutes <> " m"
     else display wholeHours <> " h " <> display fracMinutes <> " m " <> display fracSeconds <> " s"

--------------------------------------------------------------------------------
--  Internal
--------------------------------------------------------------------------------

durationHours :: Duration -> Double
durationHours (Hours t) = t
