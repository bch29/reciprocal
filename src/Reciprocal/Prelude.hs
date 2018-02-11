{-# OPTIONS_GHC -Wno-orphans #-}

module Reciprocal.Prelude
  (
    -- * Data
    -- ** Concrete Data
    module Void
  , module Text
  , module ByteString
  , module Vector
  , URI.URI
    -- ** Classes
  , module Typeable
  , module Generics
  , module String
  , module Hashable
  , Aeson.ToJSON, Aeson.FromJSON

    -- * Combinators
  , module Monad
  , module MonadIO
  , module Applicative
  , module Semigroup

    -- * Streaming
  , module Streaming

    -- * Lens
  , module Lens

    -- * Singletons
  , module Singletons

    -- * Other
  , Some(..)
  ) where

--------------------------------------------------------------------------------
--  Data
--------------------------------------------------------------------------------

-- Real data
import Data.Void as Void (Void, absurd)
import Data.Text as Text (Text)
import Data.ByteString as ByteString (ByteString)
import Data.Vector as Vector (Vector)
import Data.Vector.Generic as Vector ((!))
import qualified Text.URI as URI

-- Classes
import Data.Typeable as Typeable (Typeable, cast, gcast)
import GHC.Generics as Generics (Generic)
import Data.Hashable as Hashable (Hashable)
import Data.String as String (IsString)
import qualified Data.Aeson as Aeson

--------------------------------------------------------------------------------
--  Combinators
--------------------------------------------------------------------------------

import Control.Monad as Monad (void, guard, when, unless)
import Control.Monad.IO.Class as MonadIO (MonadIO(liftIO))
import Control.Applicative as Applicative (Alternative(..))
import Data.Semigroup as Semigroup (Semigroup(..))

--------------------------------------------------------------------------------
--  Streaming
--------------------------------------------------------------------------------

import Streaming (Stream, Of(..))

--------------------------------------------------------------------------------
--  Lens
--------------------------------------------------------------------------------

import Control.Lens as Lens hiding ((:>))
import Data.Text.Lens as Lens
import Data.ByteString.Lens as Lens

--------------------------------------------------------------------------------
--  Singletons
--------------------------------------------------------------------------------

import Data.Singletons as Singletons (SingI(..), Sing(..))

--------------------------------------------------------------------------------
--  Non-public imports
--------------------------------------------------------------------------------

import Type.Class.Higher (Show1(..))

--------------------------------------------------------------------------------
--  Other
--------------------------------------------------------------------------------

data Some k where
  Some :: Typeable a => k a -> Some k

instance Show1 k => Show (Some k) where
  showsPrec p (Some x) =
    showString "Some " .
    showParen (p > 10) (showsPrec1 11 x)

instance Aeson.ToJSON URI.URI where
  toJSON = Aeson.toJSON . URI.render

instance Aeson.FromJSON URI.URI where
  parseJSON v = do
    txt :: Text <- Aeson.parseJSON v
    case URI.mkURI txt of
      Just r -> return r
      Nothing -> fail $ "Invalid URI " <> show txt
