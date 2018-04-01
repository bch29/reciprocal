module Reciprocal.Interaction.Browse where

import Reciprocal.Prelude

import Reciprocal.Interaction.Core
import Reciprocal.Model.Recipe
import Reciprocal.Database
import Reciprocal.Interaction.Recipe

import qualified Streaming.Prelude as S


initialBrowseView :: (Monad m) => Interaction m BrowseView
initialBrowseView = return $ BrowseView
  { recipes = []
  , searchTerm = ""
  }


updateSearchTerm :: (Monad m) => Text -> BrowseView -> Interaction m BrowseView
updateSearchTerm newSearch browseView = do
  rhandler <- view (field @"recipeHandler")
  logWarning $ "Updating search term to " <> display newSearch

  let resStream = find rhandler newSearch

  results :> _ <- lift $ S.toList resStream
  logWarning $ "Got " <> display (length results) <> " results"

  return (browseView & field @"recipes" .~ results & field @"searchTerm" .~ newSearch)


selectRecipe :: (Monad m) => Key Recipe -> BrowseView -> Interaction m (Maybe RecipeView)
selectRecipe key _ = do
  rhandler <- view (field @"recipeHandler")

  mrecipe <- lift $ load rhandler key
  case mrecipe of
    Left NoSuchObject -> do
      logWarning $ "can't find that recipe!"
      return Nothing
    Left (MalformedData err) -> do
      logWarning err
      return Nothing

    Right r -> Just <$> viewRecipe r
