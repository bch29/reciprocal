{-# LANGUAGE TemplateHaskell #-}

module Reciprocal.Frontend.Terminal.Core where

import           Reciprocal.Prelude

import           Reciprocal.Interaction.Core
import           Reciprocal.Model.Recipe


import qualified Brick.Widgets.Edit            as WE
import qualified Brick.Widgets.List            as WL
-- import Brick.Forms (Form)

--------------------------------------------------------------------------------
--  Main Types
--------------------------------------------------------------------------------

data Name
  = BrowseViewList
  | BrowseViewSearchBox
  deriving (Eq, Ord, Show)

data Event = Event
  deriving (Eq, Ord, Show)

data StateBrowsing = StateBrowsing
  { _stateBrowsingBrowseView :: BrowseView
  , _stateBrowsingIsTyping   :: Bool
  , _stateBrowsingBrowseList :: WL.List Name Recipe
  , _stateBrowsingSearchBox  :: WE.Editor Text Name
  }
  deriving (Show)

data State = SBrowsing StateBrowsing
  deriving (Show)

makeFields ''StateBrowsing
makePrisms ''State

--------------------------------------------------------------------------------
--  Util
--------------------------------------------------------------------------------

interaction :: Env (ResourceT IO) -> Interaction (ResourceT IO) a -> IO a
interaction r = runResourceT . flip runReaderT r . getInteraction
