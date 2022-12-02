module Main (main) where
import Lib

main :: IO ()
main = do
  str <- readFile "input"
  print $ solve str line  -- 1st part
  print $ solve str line' -- 2st part