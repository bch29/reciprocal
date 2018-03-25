module Reciprocal.Frontend.OrgMenu where

import Reciprocal.Prelude
import Reciprocal.Model.Recipe

import Data.Text as Text

data Menu = Menu
  { _menuRecipes :: [Recipe]
  }


findMenuLinks :: Text -> [Text]
findMenuLinks = Just . Menu . goOut . concatMap (Text.splitOn "]]") .Text.splitOn "[["
  where
    goOut (_ : lnk : )
