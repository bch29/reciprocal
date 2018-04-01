module Reciprocal.Parser.General where

import           Reciprocal.Prelude         hiding (many, some)

import           Text.Megaparsec

import           Reciprocal.Model.Measure
import           Reciprocal.Model.Recipe

import           Reciprocal.Parser.Core
import           Reciprocal.Parser.Duration
import           Reciprocal.Parser.Lexer

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

parseRecipeIngredient :: Parser RecipeIngredient
parseRecipeIngredient = do
  option () (void bullet)
  m <- parseMeasureRange

  ingredientName <- unseparatedText

  attrs <- many $ try separator >> parseIngredientAttribute

  let res = RecipeIngredient (Left ingredientName) m attrs

  return res

parseIngredientAttribute :: Parser IngredientAttribute
parseIngredientAttribute = choice
  [ try $ parseWithFineness (word "chopped") (const Chopped)
  , try $ parseWithFineness (word "diced") (const Diced)
  , try $ parseWithFineness (word "sliced") (const Sliced)
  , try $ parseWithFineness (word "minced") (const Minced)
  , OtherAttr <$> unseparatedText
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

castMeasure :: (Typeable t, Typeable t') => Measure t -> Maybe (Measure t')
castMeasure = gcast

parseMeasureRange :: Parser (Some MeasureRange)
parseMeasureRange = choice
  [ try $ do
      ar <- parseAmountRange
      space
      Some u <- option (Some Whole) parseUnit
      return . Some $ case ar of
        Left x       -> SingleMR (Measure x u)
        Right (x, y) -> RangeMR (Measure x u) (Measure y u)
  , do Some (lower :: Measure t) <- parseMeasure
       higher <- option Nothing $ do
         _ <- symbol "-"
         Some (h :: Measure t') <- parseMeasure
         return (castMeasure h)

       return . Some $ case higher of
         Just h  -> RangeMR lower h
         Nothing -> SingleMR lower
  ]


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

parseAmountRange :: Parser (Either Rational (Rational, Rational))
parseAmountRange = do
  lower <- parseAmount
  higher <- option Nothing $ do
    _ <- symbol "-"
    Just <$> parseAmount

  return $ case higher of
    Just h  -> Right (lower, h)
    Nothing -> Left lower

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
