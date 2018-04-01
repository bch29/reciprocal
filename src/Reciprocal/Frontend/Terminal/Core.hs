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
  deriving (Eq, Ord, Show, Generic)

data Event = Event
  deriving (Eq, Ord, Show, Generic)

data StateBrowsing = StateBrowsing
  { browseView :: BrowseView
  , isTyping   :: Bool
  , browseList :: WL.List Name Recipe
  , searchBox  :: WE.Editor Text Name
  }
  deriving (Show, Generic)

data State = SBrowsing StateBrowsing
  deriving (Show, Generic)

--------------------------------------------------------------------------------
--  Util
--------------------------------------------------------------------------------

interaction :: Env (ResourceT IO) -> Interaction (ResourceT IO) a -> IO a
interaction r = runResourceT . flip runReaderT r . getInteraction
