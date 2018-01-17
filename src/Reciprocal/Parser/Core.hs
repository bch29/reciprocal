module Reciprocal.Parser.Core where

import           Reciprocal.Prelude

import           Text.Megaparsec

type Parser = Parsec Void Text
