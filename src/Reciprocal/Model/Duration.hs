module Reciprocal.Model.Duration
  ( Duration
  , _Hours
  , _Minutes
  , _Seconds
  , addDuration
  , prettyDuration
  ) where

import Reciprocal.Prelude

import           Text.Show (showString, showParen)

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

prettyDuration :: Duration -> String
prettyDuration d =
  let wholeHours = floor (d^._Hours) :: Int
      wholeMinutes = floor (d^._Minutes) :: Int
      wholeSeconds = floor (d^._Seconds) :: Int
      fracMinutes = wholeMinutes - wholeHours * 60
      fracSeconds = wholeSeconds - wholeMinutes * 60
  in if fracSeconds == 0
     then if fracMinutes == 0
          then show wholeHours ++ " h"
          else show wholeHours ++ " h " ++ show fracMinutes ++ " m"
     else show wholeHours ++ " h " ++ show fracMinutes ++ " m " ++ show fracSeconds ++ " s"

--------------------------------------------------------------------------------
--  Internal
--------------------------------------------------------------------------------

durationHours :: Duration -> Double
durationHours (Hours t) = t
