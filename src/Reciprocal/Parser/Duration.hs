{-# LANGUAGE OverloadedStrings #-}
module Reciprocal.Parser.Duration where

import           Reciprocal.Prelude

import           Text.Megaparsec
import           Text.Megaparsec.Char

import           Reciprocal.Model.Duration

import           Reciprocal.Parser.Core


hourSuffices, minuteSuffices, secondSuffices :: [Text]
hourSuffices = ["h", "hr", "hrs", "hours"]
minuteSuffices = ["m", "min", "mins", "minutes"]
secondSuffices = ["s", "sec", "secs", "seconds"]

parseDuration :: Parser Duration
parseDuration = do
  h <- try (numWithSuffix hourSuffices) <|> return 0
  m <- try (numWithSuffix minuteSuffices) <|> return 0
  s <- try (numWithSuffix secondSuffices) <|> return 0
  return $ (_Hours # h) `addDuration` (_Minutes # m) `addDuration` (_Seconds # s)

numWithSuffix :: Num a => [Text] -> Parser a
numWithSuffix suffices = do
  res <- parseInteger
  space
  void $ choice (map (try . string) suffices)
  space
  return res
