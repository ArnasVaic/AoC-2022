module Main (main) where

import Solution (solve)

main :: IO ()
main = do
  input <- readFile "example.txt"
  print $ solve input