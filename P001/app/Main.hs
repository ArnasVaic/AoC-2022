module Main (main) where
import Lib

main :: IO ()
main = do
  str <- readFile "input"
  print $ solve 1 str
  print $ solve 3 str