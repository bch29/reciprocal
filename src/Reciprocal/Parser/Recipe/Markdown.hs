{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Reciprocal.Parser.Recipe.Markdown where

import           Reciprocal.Prelude hiding (many, some)

import           Text.Megaparsec
import           Text.Megaparsec.Error     (parseErrorPretty)

import           Reciprocal.Model.Recipe

import           Reciprocal.Parser.Core
import           Reciprocal.Parser.General
import           Reciprocal.Parser.Lexer

test :: IO ()
test = do
  let fname = "/Users/brad/code/reciprocal/examples/vegan-chili.md"
  contents <- readFile fname

  case runParser parseRecipe fname contents of
    Left e  -> putStr (parseErrorPretty e ^. packed)
    Right x -> print x

parseRecipe :: Parser Recipe
parseRecipe = do
  t <- anyHeading 1

  initialDescCts <- restOfSection 2
  let initialDesc = mempty & field @"description" .~
        if initialDescCts == ""
        then Nothing
        else Just initialDescCts

  let parseComponent =
        choice . mapUntilLast try id $
        [ partialRecipe (field @"source") "Source" (Just <$> parseSource)
        , partialRecipe (field @"description") "Description" (Just <$> restOfSection 2)
        , partialRecipe (field @"duration") "Duration" (Just <$> parseRecipeDuration)
        , partialRecipe (field @"servings") "Servings" (Just <$> parseServings)
        , partialRecipe (field @"ingredients") "Ingredients"
          (bulleted (const parseRecipeIngredient))
        , partialRecipe (field @"instructions") "Instructions" (bulleted textInBullet)
        ]

  components <- many parseComponent
  eof
  return (mconcat (initialDesc : components) & (field @"title") .~ t)

--------------------------------------------------------------------------------
--  Recipe Components
--------------------------------------------------------------------------------

partialRecipe :: ASetter' Recipe a -> Text -> Parser a -> Parser Recipe
partialRecipe l idealTitle bodyParser = label (idealTitle ^. unpacked) $ do
  heading 2 idealTitle
  x <- bodyParser
  space'
  return (mempty & l .~ x)


--------------------------------------------------------------------------------
--  Markdown Combinators
--------------------------------------------------------------------------------

bulleted :: (Char -> Parser a) -> Parser [a]
bulleted p = many $ do
  c <- bullet
  res <- p c
  endline
  return res
