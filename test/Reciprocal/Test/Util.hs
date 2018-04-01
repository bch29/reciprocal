module Reciprocal.Test.Util where

import           Reciprocal.Prelude

import           Reciprocal.Model.Recipe
import           Reciprocal.Config
import           Reciprocal.Database
import           Reciprocal.Parser.Recipe.Markdown

import           Text.Megaparsec (runParser, parseErrorPretty)
import           Data.Maybe (catMaybes)


addExampleRecipes :: IO ()
addExampleRecipes = do
  handler <- mkRecipeHandler

  let fromFile fname = do
        contents <- readFile fname
        case runParser parseRecipe fname contents of
          Left e -> putStr (parseErrorPretty e ^. packed) >> return Nothing
          Right x -> return (Just x)

  let files = ["/Users/brad/code/reciprocal/examples/vegan-chili.md"]

  results <- catMaybes <$> traverse fromFile files

  runResourceT $ traverse_ (store handler) results


mkRecipeHandler :: IO (Handler (ResourceT IO) Recipe)
mkRecipeHandler = do
  conf <- defaultConfig
  db <- openDB conf
  return (getRecipeHandler db)
