module Main (main) where

import Lib

main :: IO ()
main = do
  f <- readFile "input"
  print $ solve f -- part 1
  print $ solve' f -- part 2
