module Lib where

import Data.Char
import Data.List
import Text.Parsec
import Text.Parsec.String (Parser)
import Control.Applicative (some)

line :: Parser [Int]
line = some (digitToInt <$> digit) <* newline

input :: Parser [[Int]]
input = some line

vis' :: Int -> [Int] -> [Bool]
vis' _ [] = []
vis' m (x:xs)
  | m >= x = False:(vis' m xs)
  | otherwise = True:(vis' x xs)

vish :: [Int] -> [Bool]
vish r = zipWith (||) (vis' (-1) r)  $ reverse $ vis' (-1) $ reverse r

visv :: [[Int]] -> [[Bool]]
visv r = transpose $ vish <$> transpose r

comb :: [Bool] -> [Bool] -> [Bool]
comb = zipWith (||)

solve :: String -> Either ParseError Int
solve s = do
  inp <- parse input "" s
  let mat = zipWith comb (vish <$> inp) $ visv inp
  pure $ length $ filter (==True) $ concat mat