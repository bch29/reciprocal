{-# LANGUAGE TemplateHaskell #-}

module Reciprocal.Frontend.Terminal where

import           Reciprocal.Prelude

import           Reciprocal.Config
import           Reciprocal.Database
import           Reciprocal.Interaction.Browse
import           Reciprocal.Interaction.Core
import qualified Reciprocal.Logging as L
import           Reciprocal.Model.Recipe
import           Reciprocal.Frontend.Terminal.Core
import           Reciprocal.Frontend.Terminal.Render
-- import Reciprocal.Interaction.Recipe

import           Data.Vector.Lens              (toVectorOf)

import           Brick
import qualified Brick.Widgets.Edit            as WE
import qualified Brick.Widgets.List            as WL
import qualified Graphics.Vty                  as V
-- import Brick.Forms (Form)

--------------------------------------------------------------------------------
--  Main Methods
--------------------------------------------------------------------------------

run :: Config -> IO ()
run conf = do
  db <- openDB conf
  lg <- L.fileLogger "log"
  let env = Env
        { recipeHandler = getRecipeHandler db
        , ingredientHandler = getIngredientHandler db
        , logger = lg
        }

      initialSearchTerm = "test"

  initialBrowseView' <- interaction env $
    updateSearchTerm initialSearchTerm =<< initialBrowseView

  let initialState = SBrowsing $ StateBrowsing
        { browseView = initialBrowseView'
        , isTyping = True
        , browseList = initialBrowseList initialBrowseView'
        , searchBox = WE.editorText BrowseViewSearchBox (Just 1) initialSearchTerm
        }
      -- buildVty = do
      --   v <- V.mkVty =<< V.standardIOConfig
      --   V.setMode (V.outputIface v) V.Mouse True
      --   return v

  void $ defaultMain (terminalApp env) initialState

handleEvent :: Env (ResourceT IO) -> State -> BrickEvent Name Event -> EventM Name (Next State)
handleEvent env (SBrowsing st) (VtyEvent e) = do
  case e of
    V.EvKey V.KEsc [] -> halt (SBrowsing st)
    V.EvKey (V.KChar 's') []
      | not (st ^. field @"isTyping") -> do
          let st' = st & field @"isTyping" .~ True
          continue (SBrowsing st')

    V.EvKey V.KEnter []
      | st ^. field @"isTyping" -> do
          let newSearch = mconcat $ WE.getEditContents (st ^. field @"searchBox")
          st' <- liftIO $ interaction env $ st & field @"browseView" %%~ updateSearchTerm newSearch
          continue (SBrowsing (st' & field @"isTyping" .~ False & updateBrowseList))

    ev
      | st ^. field @"isTyping" -> do
          st' <- st & field @"searchBox" %%~ WE.handleEditorEvent ev
          continue (SBrowsing st')

      | otherwise -> do
          st' <- st & field @"browseList" %%~ WL.handleListEvent ev
          continue (SBrowsing st')

  -- ml' <- case e of
    -- V.EvKey (V.KChar '+') [] ->
    --   let el = nextElement (WL.listElements l)
    --       pos = lengthOf (WL.listElementsL . traverse) l
    --   in return . Just $ WL.listInsert pos el l

    -- V.EvKey (V.KChar '-') [] ->
    --   case l^.(WL.listSelectedL) of
    --     Nothing -> return $ Just l
    --     Just i -> return . Just $ WL.listRemove i l

handleEvent _ l _ = continue l

terminalApp :: Env (ResourceT IO) -> App State Event Name
terminalApp env = App
  { appDraw = drawUI
  , appChooseCursor = showFirstCursor
  , appHandleEvent = handleEvent env
  , appStartEvent = return
  , appAttrMap = mkAttrMap
  }

initialBrowseList :: BrowseView -> WL.List Name Recipe
initialBrowseList bv = WL.list BrowseViewList (toVectorOf (field @"recipes".traverse) bv) 1

updateBrowseList :: StateBrowsing -> StateBrowsing
updateBrowseList st = st & field @"browseList" .~ initialBrowseList (st ^. field @"browseView")
