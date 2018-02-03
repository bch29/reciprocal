module Reciprocal.Parser.Core where

import           Reciprocal.Prelude

import           Text.Megaparsec
import           Text.Megaparsec.Char

type Parser = Parsec Void Text

parseInteger :: Num a => Parser a
parseInteger = do
  digits <- many digitChar
  return (fromIntegral (read digits :: Integer))
