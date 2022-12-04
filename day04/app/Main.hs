module Main (main) where
import Lib

main :: IO ()
main = do
  str <- readFile "input"
  print $ solve str contains  -- 1st part
  print $ solve str overlap   -- 2nd part