module Main (main) where
import Lib

main :: IO ()
main = do
  str <- readFile "input"
  print $ solve 1 str -- 1st part
  print $ solve 3 str -- 2nd part