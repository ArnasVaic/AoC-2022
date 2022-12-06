module Main (main) where
import Lib

main :: IO ()
main = do
  str <- readFile "input"
  print $ solve str 4  -- 1st part
  print $ solve str 14 -- 2nd part