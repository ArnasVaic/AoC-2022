module Main (main) where
import Lib

main :: IO ()
main = do
  str <- readFile "input"
  print $ solve str -- 1st part