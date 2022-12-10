module Main (main) where

import Solution (solve)

main :: IO ()
main = do
  input <- readFile "input"
  print $ solve input
