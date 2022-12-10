module Main (main) where

import Solution (solve, solve')

main :: IO ()
main = do
  input <- readFile "input"
  print $ solve input
  putStrLn $ solve' input
