module Main (main) where
import Lib

main :: IO ()
main = do
  str <- readFile "input"
  print $ solve str pair  -- 1st part
  print $ solve str pair' -- 2nd part