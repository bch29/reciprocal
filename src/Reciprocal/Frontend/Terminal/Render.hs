{-# LANGUAGE TemplateHaskell #-}

module Reciprocal.Frontend.Terminal.Render where

import           Reciprocal.Prelude

import           Reciprocal.Model.Recipe
import           Reciprocal.Frontend.Terminal.Core
-- import Reciprocal.Interaction.Recipe

import qualified Data.Text                     as Text

import           Brick
import qualified Brick.Widgets.Border          as WB
import qualified Brick.Widgets.Center          as WC
import qualified Brick.Widgets.Edit            as WE
import qualified Brick.Widgets.List            as WL
import qualified Graphics.Vty                  as V
-- import Brick.Forms (Form)

drawUI :: State -> [Widget Name]
drawUI (SBrowsing st) = drawBrowsing st

drawBrowsing :: StateBrowsing -> [Widget Name]
drawBrowsing st = [ui]
  where
    l = st ^. browseList

    label = txt "Search results"

    box = WB.borderWithLabel label $
      hLimit 100 $
      -- vLimit 15 $
      WL.renderList displayRecipe True l

    helpMsg = if st ^. isTyping
      then
      [ WC.hCenter $ txt "Press enter to search."
      , WC.hCenter $ txt "Press Esc to exit."
      ]
      else
      [ WC.hCenter $ txt "Use the arrow keys to navigate."
      , WC.hCenter $ txt "Press Enter to view or edit a recipe."
      , WC.hCenter $ txt "Press s to change the search term."
      , WC.hCenter $ txt "Press Esc to exit."
      ]

    ui = WC.vCenter $ vBox $
         [ WC.hCenter $ renderSearchBox st (st ^. searchBox)
         , WC.hCenter box
         , txt " "
         ] ++ helpMsg

renderSearchBox :: StateBrowsing -> WE.Editor Text Name -> Widget Name
renderSearchBox st editor = WB.border $ hLimit 100 $
  txt "Search: " <+>
  WE.renderEditor (txt . Text.unlines) (st ^. isTyping) editor

mkAttrMap :: State -> AttrMap
mkAttrMap _ = attrMap V.defAttr
    [ (WL.listAttr,            V.white `on` V.blue)
    , (WL.listSelectedAttr,    V.blue `on` V.white)
    , (selRecipeAttr,          fg V.cyan)
    ]

selRecipeAttr :: AttrName
selRecipeAttr = WL.listSelectedAttr `mappend` "recipe"

displayRecipe :: Bool -> Recipe -> Widget Name
displayRecipe sel r =
  let selStr s =
        if sel
        then withAttr selRecipeAttr (txt $ "<" <> s <> ">")
        else txt s
  in selStr $ (r ^. title)
