module Main (main) where
import Lib

main :: IO ()
main = do
  str <- readFile "input"
  putStrLn $ show (solve 1 str)
  putStrLn $ show (solve 3 str)