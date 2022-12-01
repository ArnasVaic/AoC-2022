module Main (main) where

import Text.Parsec ( ParseError )

import System.IO
import Control.Monad.IO.Class

import Lib

main :: IO (Either ParseError Integer)
main = do
  str <- readFile "input"
  pure $ solve str