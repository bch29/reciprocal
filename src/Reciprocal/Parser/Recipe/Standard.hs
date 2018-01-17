{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Reciprocal.Parser.Recipe.Standard where

import           Reciprocal.Prelude

import           Data.Char                    (GeneralCategory (Space, LineSeparator, ParagraphSeparator))
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified URI.ByteString               as URI

import           Reciprocal.Model.Food.Recipe

import           Reciprocal.Parser.Core
import           Reciprocal.Parser.Duration

parseRecipe :: Parser Recipe
parseRecipe = do
  titleStart 1
  t <- restOfLine

  initialDescCts <- restOfSection 2
  let initialDesc = mempty & description .~
        if initialDescCts == ""
        then Nothing
        else Just initialDescCts

  let parseComponent =
        choice
        [ partialRecipe source "Source" (Just <$> parseSource)
        , partialRecipe description "Description" (Just <$> restOfSection 2)
        , partialRecipe duration "Duration" (Just <$> parseRecipeDuration)
        , partialRecipe ingredients "Ingredients" parseRecipeIngredients
        , partialRecipe instructions "Instructions" parseRecipeInstructions
        ]

  components <- many parseComponent
  return (mconcat (initialDesc : components) & title .~ t)

--------------------------------------------------------------------------------
--  Recipe Components
--------------------------------------------------------------------------------

partialRecipe :: ASetter' Recipe a -> Text -> Parser a -> Parser Recipe
partialRecipe l idealTitle bodyParser = do
  _ <- string idealTitle
  space
  x <- bodyParser
  return (mempty & l .~ x)

parseSource :: Parser RecipeSource
parseSource = do
  body <- restOfSection 2
  case URI.parseURI URI.laxURIParserOptions (body ^. unpacked . packedChars) of
    Left _ -> return (SourceOther body)
    Right uri -> return (SourceWebsite uri)

parseRecipeDuration :: Parser RecipeDuration
parseRecipeDuration = choice
  [ try $ do
      activeDur <- parseDuration
      space
      void $ string "active"
      space
      void $ char ','
      space
      totalDur <- parseDuration
      space
      void $ string "total"
      return (ActiveTotal activeDur totalDur)
  , Undivided <$> parseDuration
  ]

parseRecipeInstructions :: Parser [Text]
parseRecipeInstructions = sepBy (view packed <$> many anyChar) (endline >> char '-' >> space)

parseRecipeIngredients :: Parser [RecipeIngredient]
parseRecipeIngredients = sepBy parseRecipeIngredient endline

parseRecipeIngredient :: Parser RecipeIngredient
parseRecipeIngredient = undefined


--------------------------------------------------------------------------------
--  Markdown Combinators
--------------------------------------------------------------------------------

titleStart :: Int -> Parser ()
titleStart depth = void $ count depth (char '#') >> space'

-- | Parses the rest of the section at the given depth. Returns the section's
-- text, minus any trailing whitespace.
restOfSection :: Int -> Parser Text
restOfSection nextDepth = do
  sectionChars <- manyTill anyChar (titleStart nextDepth >> space)
  return (sectionChars ^. packed)

--------------------------------------------------------------------------------
--  General Combinators
--------------------------------------------------------------------------------

-- | Parses zero or more horizontal spaces (i.e. not newlines or paragraph separators).
space' :: Parser [Char]
space' = many $ charCategory Space

restOfLine :: Parser Text
restOfLine = do
  lineChars <- manyTill anyChar endline
  space
  return (lineChars ^. packed)

endline :: Parser Char
endline = charCategory LineSeparator <|> charCategory ParagraphSeparator
