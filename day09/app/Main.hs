module Main (main) where

import Solution (solve) 

main :: IO ()
main = do 
  input <- readFile "input"
  case solve input of
    Nothing -> error "Parse error occured"
    Just s -> print s