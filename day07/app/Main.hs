module Main (main) where

import Lib2

main :: IO ()
main = do
  fn <- getLine
  str <- readFile fn
  print $ solve str
  --print $ solve str -- 2nd part