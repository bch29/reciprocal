{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Reciprocal.Parser.Recipe.Standard where

import           Reciprocal.Prelude

import           Data.Ratio                 ((%))
import qualified URI.ByteString             as URI

import           Data.Char                  (GeneralCategory (Space, LineSeparator, ParagraphSeparator))
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Control.Monad.Combinators.NonEmpty as NE

import           Reciprocal.Model.Measure
import           Reciprocal.Model.Recipe

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

parseMeasure :: Parser (Some Measure)
parseMeasure = undefined

parse1Measure :: Parser (Some Measure)
parse1Measure = choice
  [ try $ do
      x <- parseAmount
      space
      Some u <- parseUnit
      return $ Some $ Measure x u
  ]

parseAmount :: Parser Rational
parseAmount = choice
  [
    -- Just an integer
    try parseInteger
    -- Just a decimal number
  , try $ do
      wholePart <- parseInteger
      void $ char '.'
      fractionDigits <- many digitChar
      return $ wholePart + read fractionDigits / (10^length fractionDigits)
    -- Just a fraction
  , try parseFraction
    -- Whole part plus fractional part (i.e. mixed number)
  , try $ do
      wholePart <- parseInteger
      void $ some spaceChar
      fractionalPart <- parseFraction
      return (wholePart + fractionalPart)
  ]

parseFraction :: Parser Rational
parseFraction = try $ do
  numerator <- parseInteger
  space
  void $ char '/'
  space
  denominator <- parseInteger
  return $ numerator % denominator


parseUnit :: Parser (Some Unit)
parseUnit = undefined

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
