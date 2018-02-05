{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Reciprocal.Parser.Recipe.Standard where

import           Reciprocal.Prelude

import qualified Data.Text as Text
import           Text.Megaparsec
import           Text.Megaparsec.Error      (parseErrorPretty)

import           Debug.Trace                (traceM, traceShowM)

import           Reciprocal.Model.Measure
import           Reciprocal.Model.Recipe

import           Reciprocal.Parser.Core
import           Reciprocal.Parser.Duration
import           Reciprocal.Parser.Lexer

test :: IO ()
test = do
  let fname = "/home/brad/code/reciprocal/examples/vegan-chili.md"
  contents :: Text <- view packed <$> readFile fname

  case runParser parseRecipe fname contents of
    Left e  -> putStr (parseErrorPretty e)
    Right x -> print x

parseRecipe :: Parser Recipe
parseRecipe = do
  traceM "Looking for title"
  t <- anyHeading 1

  traceM $ "Got title '" ++ (t ^. unpacked) ++ "'"

  initialDescCts <- restOfSection 2
  let initialDesc = mempty & description .~
        if initialDescCts == ""
        then Nothing
        else Just initialDescCts

  let parseComponent =
        choice . mapUntilLast try id $
        [ partialRecipe source "Source" (Just <$> parseSource)
        , partialRecipe description "Description" (Just <$> restOfSection 2)
        , partialRecipe duration "Duration" (Just <$> parseRecipeDuration)
        , partialRecipe servings "Servings" (Just <$> parseServings)
        , partialRecipe ingredients "Ingredients" parseRecipeIngredients
        , partialRecipe instructions "Instructions" parseRecipeInstructions
        ]

  components <- many parseComponent
  eof
  return (mconcat (initialDesc : components) & title .~ t)

--------------------------------------------------------------------------------
--  Recipe Components
--------------------------------------------------------------------------------

partialRecipe :: ASetter' Recipe a -> Text -> Parser a -> Parser Recipe
partialRecipe l idealTitle bodyParser = label (idealTitle ^. unpacked) $ do
  traceM $ "Trying to parse " ++ (idealTitle ^. unpacked)
  heading 2 idealTitle
  x <- bodyParser
  traceM $ "Got body for " ++ (idealTitle ^. unpacked)
  return (mempty & l .~ x)

parseSource :: Parser RecipeSource
parseSource = try (SourceWebsite <$> uri) <|> (SourceOther <$> restOfSection 2)

parseServings :: Parser Rational
parseServings = number

parseRecipeDuration :: Parser RecipeDuration
parseRecipeDuration = choice
  [ try $ do
      activeDur <- parseDuration
      void $ word "active"
      void $ separator
      totalDur <- parseDuration
      void $ word "total"
      return (ActiveTotal activeDur totalDur)
  , Undivided <$> parseDuration
  ]

parseRecipeInstructions :: Parser [Text]
parseRecipeInstructions =
  sepBy
    (Text.unwords <$> many (anyWord <|> separator))
    (try (endline >> lookAhead bullet))

parseRecipeIngredients :: Parser [RecipeIngredient]
parseRecipeIngredients = sepBy parseRecipeIngredient endline

parseRecipeIngredient :: Parser RecipeIngredient
parseRecipeIngredient = do
  option () (void bullet)
  m <- parseMeasure

  ingredientName <- Text.unwords <$> some anyWord

  attrs <- option [] $ separator >> sepBy parseIngredientAttribute separator

  let res = RecipeIngredient (Left ingredientName) m attrs
  traceShowM res

  return res

parseIngredientAttribute :: Parser IngredientAttribute
parseIngredientAttribute = choice
  [ try $ parseWithFineness (word "chopped") (const Chopped)
  , try $ parseWithFineness (word "diced") (const Diced)
  , try $ parseWithFineness (word "sliced") (const Sliced)
  , try $ parseWithFineness (word "minced") (const Minced)
  , OtherAttr . Text.unwords <$> many anyWord
  ]

parseWithFineness :: Parser a -> (a -> IngredientFineness -> b) -> Parser b
parseWithFineness p k = choice
  [ try $ do
      f <- parseIngredientFineness
      space
      x <- p
      return (k x f)
  , do
      x <- p
      space
      f <- parseIngredientFineness
      return (k x f)
  ]

parseIngredientFineness :: Parser IngredientFineness
parseIngredientFineness =
  tryWords
    [ (["very finely", "very fine"], VeryFine)
    , (["finely", "fine"], Fine)
    , (["roughly", "coarsely", "rough", "coarse"], Rough)
    , ( ["very roughly", "very coarsely", "very rough", "very coarse"]
      , VeryRough)
    ]


-- TODO: Handle ranges of measures
parseMeasure :: Parser (Some Measure)
parseMeasure = choice
  [ do x <- parseAmount
       space
       Some u <- option (Some Whole) parseUnit
       return $ Some $ Measure x u
  ]

-- TODO: fractions and mixed numbers
parseAmount :: Parser Rational
parseAmount = number

-- parseFraction :: Parser Rational
-- parseFraction = do
--   numerator <- parseInteger
--   space
--   void $ char '/'
--   space
--   denominator <- parseInteger
--   return $ numerator % denominator


parseUnit :: Parser (Some Unit)
parseUnit =
  tryWords
    [ (["litres", "litre", "liters", "liter", "l"], Some Litres)
    , (["millilitres", "millilitre", "milliliters", "milliliter", "ml"], Some (milli Litres))
    , (["grams", "gram", "g"], Some Grams)
    , (["kilograms", "kilogram", "kg"], Some (kilo Grams))
    , (["teaspoons", "teaspoon", "tsp"], Some Teaspoons)
    , (["tablespoons", "tablespoon", "tbsp"], Some Tablespoons)
    , (["whole"], Some Whole)
    , (["cloves", "clove"], Some Cloves)
    ]

--------------------------------------------------------------------------------
--  General Combinators
--------------------------------------------------------------------------------

tryWords :: [([Text], a)] -> Parser a
tryWords =
  choice .
  mapUntilLast id try .
  map (\(s, x) -> word s >> space >> return x) .
  concatMap (\(s, x) -> zip s (repeat x))

mapUntilLast :: (a -> b) -> (a -> b) -> [a] -> [b]
mapUntilLast _ _ []       = []
mapUntilLast _ g [x]      = [g x]
mapUntilLast f g (x : xs) = f x : mapUntilLast f g xs
