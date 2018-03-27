{-# LANGUAGE TemplateHaskell #-}

module Reciprocal.Frontend.Terminal where

import Reciprocal.Prelude

import Reciprocal.Interaction.Core
import Reciprocal.Interaction.Browse
import Reciprocal.Interaction.Recipe
import Reciprocal.Database
import Reciprocal.Config
import Reciprocal.Logging
import Reciprocal.Model.Recipe

import Data.Vector.Lens (toVectorOf)

import qualified Graphics.Vty as V
import Brick
import qualified Brick.Widgets.List as WL
import qualified Brick.Widgets.Border as WB
import qualified Brick.Widgets.Center as WC
-- import Brick.Forms (Form)

--------------------------------------------------------------------------------
--  Main Types
--------------------------------------------------------------------------------

data Name = BrowseViewList
  deriving (Eq, Ord, Show)

data Event = Event
  deriving (Eq, Ord, Show)

data State = State
  { _stateBrowseView :: BrowseView
  , _stateBrowseList :: WL.List Name Recipe
  }
  deriving (Show)

makeFields ''State

--------------------------------------------------------------------------------
--  Main Methods
--------------------------------------------------------------------------------

run :: Config -> IO ()
run conf = do
  db <- openDB conf
  let env = Env
        { _envRecipeHandler = getRecipeHandler db
        , _envIngredientHandler = getIngredientHandler db
        , _envLogger = stdioLogger
        }

  initialBrowseView' <- interaction env $
    updateSearchTerm "Vegan Chili" =<< initialBrowseView

  let initialState = State
        { _stateBrowseView = initialBrowseView'
        , _stateBrowseList = browseViewList initialBrowseView'
        }
      -- buildVty = do
      --   v <- V.mkVty =<< V.standardIOConfig
      --   V.setMode (V.outputIface v) V.Mouse True
      --   return v

  void $ defaultMain terminalApp initialState

drawUI :: State -> [Widget Name]
drawUI st = [ui]
  where
    l = st ^. browseList

    label = str "Item " <+> cur <+> str " of " <+> total

    cur = case l^.(WL.listSelectedL) of
            Nothing -> str "-"
            Just i -> str (display (i + 1) ^. unpacked)

    total = str $ display (lengthOf (WL.listElementsL . traverse) l) ^. unpacked

    box = WB.borderWithLabel label $
      hLimit 25 $
      vLimit 15 $
      WL.renderList displayRecipe True l

    ui = WC.vCenter $ vBox
         [ WC.hCenter box
         , str " "
         , WC.hCenter $ str "Press +/- to add/remove list elements."
         , WC.hCenter $ str "Press Esc to exit."
         ]


handleEvent :: State -> BrickEvent Name Event -> EventM Name (Next State)
handleEvent st (VtyEvent e) = do
  let l = st ^. browseList

  case e of
    V.EvKey V.KEsc [] -> halt st

    ev -> do
      l' <- WL.handleListEvent ev l
      continue (st & browseList .~ l')

  -- ml' <- case e of
    -- V.EvKey (V.KChar '+') [] ->
    --   let el = nextElement (WL.listElements l)
    --       pos = lengthOf (WL.listElementsL . traverse) l
    --   in return . Just $ WL.listInsert pos el l

    -- V.EvKey (V.KChar '-') [] ->
    --   case l^.(WL.listSelectedL) of
    --     Nothing -> return $ Just l
    --     Just i -> return . Just $ WL.listRemove i l

handleEvent l _ = continue l

mkAttrMap :: State -> AttrMap
mkAttrMap _ = attrMap V.defAttr
    [ (WL.listAttr,            V.white `on` V.blue)
    , (WL.listSelectedAttr,    V.blue `on` V.white)
    , (selRecipeAttr,             fg V.cyan)
    ]

terminalApp :: App State Event Name
terminalApp = App
  { appDraw = drawUI
  , appChooseCursor = showFirstCursor
  , appHandleEvent = handleEvent
  , appStartEvent = return
  , appAttrMap = mkAttrMap
  }

selRecipeAttr :: AttrName
selRecipeAttr = WL.listSelectedAttr `mappend` "recipe"

displayRecipe :: Bool -> Recipe -> Widget Name
displayRecipe sel r =
  let selStr s =
        if sel
        then withAttr selRecipeAttr (str $ "<" <> s <> ">")
        else str s
  in str "Item " <+> (selStr $ (r ^. title.unpacked))

browseViewList :: BrowseView -> WL.List Name Recipe
browseViewList bv = WL.list BrowseViewList (toVectorOf (recipes.traverse) bv) 1

--------------------------------------------------------------------------------
--  Util
--------------------------------------------------------------------------------

interaction :: Env (ResourceT IO) -> Interaction (ResourceT IO) a -> IO a
interaction r = runResourceT . flip runReaderT r . getInteraction
