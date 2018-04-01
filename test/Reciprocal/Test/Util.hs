module Reciprocal.Test.Util where

import           Reciprocal.Prelude

import           Reciprocal.Model.Recipe
import           Reciprocal.Config
import           Reciprocal.Database
import           Reciprocal.Parser.Recipe.Markdown

import           Text.Megaparsec (runParser, parseErrorPretty)
import           Data.Maybe (catMaybes)
import           System.Environment (getEnv)
import           System.FilePath ((</>))
import           System.Exit (exitWith, ExitCode(ExitFailure))


addExampleRecipes :: IO ()
addExampleRecipes = do
  handler <- mkRecipeHandler

  let fromFile fname = do
        contents <- readFile fname
        case runParser parseRecipe fname contents of
          Left e -> putStr (parseErrorPretty e ^. packed) >> return Nothing
          Right x -> return (Just x)

  homeDir <- getEnv "HOME"
  let files = [homeDir </> "code/reciprocal/examples/vegan-chili.md"]

  results <- catMaybes <$> traverse fromFile files

  runResourceT $ traverse_ (store handler) results

getVeganChili :: IO Recipe
getVeganChili = do
  handler <- mkRecipeHandler
  let key = UnsafeMkKey "Vegan Chili.json"
  res <- runResourceT $ load handler key
  case res of
    Left e -> putStrLn (display e) >> exitWith (ExitFailure 1)
    Right x -> return x


mkRecipeHandler :: IO (Handler (ResourceT IO) Recipe)
mkRecipeHandler = do
  conf <- defaultConfig
  db <- openDB conf
  return (getRecipeHandler db)
