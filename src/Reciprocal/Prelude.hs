{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
module Reciprocal.Prelude
  (
    -- * Data
    Void
  , Text.Text
  , ByteString.ByteString

    -- * Combinators
  , module Monad
  , module Applicative
  , module Semigroup

    -- * Lens
  , module Lens

    -- * Other
  , Some(..)
  ) where

--------------------------------------------------------------------------------
--  Data
--------------------------------------------------------------------------------

import Data.Void (Void)
import Data.Text as Text
import Data.ByteString as ByteString

--------------------------------------------------------------------------------
--  Combinators
--------------------------------------------------------------------------------

import Control.Monad as Monad (void, guard)
import Control.Applicative as Applicative (Alternative(..))
import Data.Semigroup as Semigroup (Semigroup(..))

--------------------------------------------------------------------------------
--  Lens
--------------------------------------------------------------------------------

import Control.Lens as Lens
import Data.Text.Lens as Lens
import Data.ByteString.Lens as Lens

--------------------------------------------------------------------------------
--  Non-public imports
--------------------------------------------------------------------------------

import Type.Class.Higher (Show1(..))

--------------------------------------------------------------------------------
--  Other
--------------------------------------------------------------------------------

data Some k where
  Some :: k a -> Some k

instance Show1 k => Show (Some k) where
  showsPrec p (Some x) =
    showString "Some " .
    showParen (p > 10) (showsPrec1 11 x)
