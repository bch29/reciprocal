module Reciprocal.Parser.Lexer where

import Reciprocal.Prelude
import Reciprocal.Parser.Core

import qualified Text.URI as URI

import           Text.Megaparsec
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L

--------------------------------------------------------------------------------
--  Generic
--------------------------------------------------------------------------------

sepChars :: [Char]
sepChars = [',', ';']

bulletChars :: [Char]
bulletChars = ['-', '*']

space :: Parser ()
space = L.space C.space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

symbol :: Text -> Parser Text
symbol = L.symbol space

startWord :: Parser Char
startWord = C.letterChar

anyWord :: Parser Text
anyWord = label "any word" $ lexeme $ do
  firstChar <- startWord
  rest <- manyTill (C.noneOf sepChars) C.space1
  return $ (firstChar : rest) ^. packed

word :: Text -> Parser Text
word = symbol

number :: Parser Rational
number = label "number" $ lexeme $ do
  wholePart <- L.decimal
  fractionalPart <- option 0 $ do
    void $ C.char '.'
    fractionDigits <- many C.digitChar
    return (read fractionDigits / (10^length fractionDigits))

  return (fromInteger wholePart + fractionalPart)

separator :: Parser Text
separator = (view packed . (: []) <$> C.oneOf sepChars) <* space

bullet :: Parser Text
bullet = label "bullet" $ view packed . (: []) <$> lexeme (C.oneOf bulletChars)

uri :: Parser URI.URI
uri = label "uri" $ lexeme URI.parser

endline :: Parser ()
endline = lexeme . void $ C.char '\n'

restOfLine :: Parser Text
restOfLine = label "rest of line" $ view packed <$> lexeme (manyTill C.anyChar (lookAhead endline))

restOfSection :: Int -> Parser Text
restOfSection depth = label "rest of section" $ lexeme $ do
  sectionChars <- manyTill C.anyChar (try (lookAhead (space >> headingStart depth)))
  return (sectionChars ^. packed)

--------------------------------------------------------------------------------
--  Markdown
--------------------------------------------------------------------------------

anyHeading :: Int -> Parser Text
anyHeading depth = label ("any heading at depth " ++ show depth) $ headingStart depth >> restOfLine

heading :: Int -> Text -> Parser ()
heading depth hd = label ("heading at depth " ++ show depth) $ headingStart depth >> symbol hd >> return ()

headingStart :: Int -> Parser ()
headingStart depth = label (replicate depth '#') $ void $ symbol (replicate depth '#' ^. packed)
