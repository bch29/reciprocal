module Reciprocal.Model.Time.Duration
  ( Duration
  , _Hours
  , _Minutes
  , _Seconds
  , prettyDuration
  ) where

import Control.Lens

--------------------------------------------------------------------------------
--  Types
--------------------------------------------------------------------------------

data Duration = Hours Double

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
--  Exported Other
--------------------------------------------------------------------------------

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
