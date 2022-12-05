module Main (main) where
import Lib

main :: IO ()
main = do
  str <- readFile "input"
  print $ solve str True  -- 1st part
  print $ solve str False -- 2nd part