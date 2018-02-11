module Reciprocal.Parser.Lexer where

import           Reciprocal.Parser.Core
import           Reciprocal.Prelude

import           Data.Ratio                 ((%))

import qualified Text.URI                   as URI

import qualified Data.Char                  as Char

import           Text.Megaparsec
import qualified Text.Megaparsec.Char       as C
import qualified Text.Megaparsec.Char.Lexer as L

--------------------------------------------------------------------------------
--  Generic
--------------------------------------------------------------------------------

sepChars :: [Char]
sepChars = [',', ';']

bulletChars :: [Char]
bulletChars = ['-', '*']

horizontalSpaceChar :: Parser ()
horizontalSpaceChar = void $ C.charCategory Char.Space <|> C.char '\t'

space :: Parser ()
space = L.space (void (some horizontalSpaceChar)) empty empty

space' :: Parser ()
space' = L.space C.space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

symbol :: Text -> Parser Text
symbol = L.symbol space

startWord :: Parser Char
startWord = C.letterChar

-- | Parses text not separated by separator characters or bullet points. Text
-- may contain arbitrary whitespace (including newlines) within it, but the
-- result will not have whitespace at the beginning or end. The result will also
-- have whitespace normalized, i.e. each stretch of whitespace is replaced by a
-- single space.
unseparatedText :: Parser Text
unseparatedText = label "unseparated text" $
  normalizingWhitespace (C.alphaNumChar <|> C.oneOf ("'\"()" :: String))

textInBullet :: Char -> Parser Text
textInBullet _ = label "body of a bullet point" . normalizingWhitespace $
  choice
  [ C.alphaNumChar
  , C.oneOf ("'\"();,:/." :: String)
  , C.oneOf bulletChars <* notFollowedBy (try endline)
  ]

normalizingWhitespace :: Parser Char -> Parser Text
normalizingWhitespace wordChar = do
  space

  let aWord = some wordChar
      someSpace = some C.spaceChar

  firstWord <- aWord
  rest <- many (try (someSpace >> aWord))

  return (unwords (firstWord : rest) ^. packed)

word :: Text -> Parser Text
word = symbol

number :: Parser Rational
number = label "number" $ lexeme $ do
  wholePart <- L.decimal
  fractionalPart <- option 0 $ do
    void $ C.char '.'
    fractionDigits <- many C.digitChar
    return (read fractionDigits % (10^length fractionDigits))

  return (fromInteger wholePart + fractionalPart)

separator :: Parser Text
separator = (view packed . (: []) <$> C.oneOf sepChars) <* space

bullet :: Parser Char
bullet = label "bullet" $ lexeme (C.oneOf bulletChars)

uri :: Parser URI.URI
uri = label "uri" $ lexeme URI.parser

endline :: Parser ()
endline = void C.eol

restOfLine :: Parser Text
restOfLine = label "rest of line" $ view packed <$> lexeme (manyTill C.anyChar (lookAhead endline)) <* space'

restOfSection :: Int -> Parser Text
restOfSection depth = label "rest of section" $ do
  sectionChars <- manyTill C.anyChar (try (lookAhead (space >> headingStart depth)))
  return (sectionChars ^. packed)

--------------------------------------------------------------------------------
--  Markdown
--------------------------------------------------------------------------------

anyHeading :: Int -> Parser Text
anyHeading depth = label ("any heading at depth " ++ show depth) $ do
  headingStart depth
  restOfLine

heading :: Int -> Text -> Parser ()
heading depth hd = label ("heading at depth " ++ show depth) $ do
  headingStart depth
  _ <- symbol hd
  space'

headingStart :: Int -> Parser ()
headingStart depth = label (replicate depth '#') $ void $ symbol (replicate depth '#' ^. packed)
