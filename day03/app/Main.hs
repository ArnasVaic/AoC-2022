module Main (main) where
import Lib

main :: IO ()
main = do
  str <- readFile "input"
  print $ solve str rucksack  -- 1st part
  print $ solve str badge     -- 2nd part