module Main where

import Reciprocal.Prelude
import Reciprocal.Config
import Reciprocal.Frontend.Terminal

main :: IO ()
main = do
  conf <- defaultConfig
  run conf
