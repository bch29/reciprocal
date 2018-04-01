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
  , module MonadTrans
  , module Applicative
  , module Semigroup
  , module Foldable

    -- * IO
  , module TextIO

    -- * Monads
  , ReaderT, runReaderT
  , MaybeT, runMaybeT
  , ExceptT, runExceptT
  , MonadReader, MonadError, MonadState

    -- * Streaming
  , module Streaming
  , ConduitT

  -- ** Conduits
  , MonadResource, ResourceT, runResourceT, MonadUnliftIO
  , streamToConduit, conduitToStream

  -- , pipeToStream
  -- , streamToPipe

    -- * Lens
  , module Lens

    -- * Singletons
  , module Singletons

    -- * Other
  , display
  , Some(..)

    -- * Re-exported from "Prelude"
  , module BasePrelude
  ) where

--------------------------------------------------------------------------------
--  Prelude Re-exports
--------------------------------------------------------------------------------

import Prelude as BasePrelude hiding
  (
    -- Purge 'String'!
    String
  , putStrLn
  , putStr
  , show
  , showsPrec
  , showString

  , writeFile
  , readFile
  , appendFile
  , getContents
  , getLine
  , interact
  )

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

import Control.Monad as Monad (void, guard, when, unless, join)
import Control.Monad.IO.Class as MonadIO (MonadIO(liftIO))
import Control.Monad.Trans.Class as MonadTrans (MonadTrans(..))
import Control.Applicative as Applicative (Alternative(..))
import Data.Semigroup as Semigroup (Semigroup(..))
import Data.Foldable as Foldable (traverse_)

--------------------------------------------------------------------------------
--  IO
--------------------------------------------------------------------------------

import Data.Text.IO as TextIO

--------------------------------------------------------------------------------
--  Monads
--------------------------------------------------------------------------------

import Control.Monad.Reader (ReaderT, runReaderT, MonadReader)
import Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import Control.Monad.Except (ExceptT, runExceptT, MonadError)
import Control.Monad.State.Class (MonadState)

--------------------------------------------------------------------------------
--  Streaming
--------------------------------------------------------------------------------

import Streaming (Stream, Of(..))
import Conduit (ConduitT)
import Control.Monad.Trans.Resource
  ( ResourceT
  , MonadResource
  , runResourceT
  , MonadUnliftIO)

--------------------------------------------------------------------------------
--  Lens
--------------------------------------------------------------------------------

import Control.Lens.At as Lens
-- import Control.Lens.Cons as Lens
import Control.Lens.Each as Lens
import Control.Lens.Empty as Lens
import Control.Lens.Equality as Lens
import Control.Lens.Fold as Lens
import Control.Lens.Getter as Lens
import Control.Lens.Indexed as Lens
import Control.Lens.Iso as Lens
import Control.Lens.Lens as Lens
import Control.Lens.Level as Lens
import Control.Lens.Plated as Lens
import Control.Lens.Prism as Lens
import Control.Lens.Reified as Lens
import Control.Lens.Review as Lens
import Control.Lens.Setter as Lens
import Control.Lens.Traversal as Lens
import Control.Lens.Tuple as Lens
import Control.Lens.Type as Lens
import Control.Lens.Wrapped as Lens
import Control.Lens.Zoom as Lens

import Data.Generics.Product.Fields as Lens
import Data.Generics.Sum.Constructors as Lens
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
import qualified Conduit as Conduit
import qualified Streaming.Prelude as Streaming
import qualified Streaming as Streaming

--------------------------------------------------------------------------------
--  Other
--------------------------------------------------------------------------------

import Text.Show (show, showsPrec, showString)

-- | Regular 'show' except it outputs 'Text'.
display :: Show a => a -> Text
display = view packed . show

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


conduitToStream :: (Monad m) => ConduitT () a m () -> Stream (Of a) m ()
conduitToStream = Streaming.unfoldr next . Conduit.sealConduitT
  where
    next
      :: (Monad m)
      => Conduit.SealedConduitT () a m ()
      -> m (Either () (a, Conduit.SealedConduitT () a m ()))
    next c = do
      (c', mx) <- c Conduit.$$++ Conduit.await
      case mx of
        Just x -> return (Right (x, c'))
        Nothing -> return (Left ())


streamToConduit :: (Monad m) => Stream (Of a) m r -> ConduitT i a m r
streamToConduit = Streaming.foldrT (\a p -> Conduit.yield a >> p)

-- pipeToStream :: (Monad m) => Pipes.Producer a m r -> Stream (Of a) m r
-- pipeToStream = Streaming.unfoldr Pipes.next


-- streamToPipe :: (Monad m) => Stream (Of a) m r -> Pipes.Producer a m r
-- streamToPipe = Streaming.foldrT (\a p -> Pipes.yield a >> p)


-- streamToEffect :: (Monad m) => Stream Identity m r -> Pipes.Effect' m r
-- streamToEffect = lift . Streaming.iterT runIdentity


-- effectToStream :: (Monad m, Functor f) => Pipes.Effect m r -> Stream f m r
-- effectToStream = lift . Pipes.runEffect


-- runSafeS :: (MonadMask m, MonadIO m, Functor f) => Stream Identity (SafeT m) r -> Stream f m r
-- runSafeS = effectToStream . Pipes.runSafeP . streamToEffect
