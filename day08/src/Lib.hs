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

-- h is current tree height
-- xs here is all the trees that follow to a certain direction
scoreElem :: Int-> [Int] -> Int
scoreElem h xs = case findIndex (>= h) xs of
  Just i -> i + 1
  Nothing -> length xs

scoreRight :: [Int] -> [Int]
scoreRight [] = []
scoreRight (x:xs) = scoreElem x xs : scoreRight xs

scoreLeft :: [Int] -> [Int]
scoreLeft= reverse. scoreRight . reverse

rowScore :: [Int] -> [Integer]
rowScore l = zipWith (*) (toInteger <$> scoreRight l) (toInteger <$> scoreLeft l)

score :: [[Int]] -> [[Integer]]
score inp = zipWith (zipWith (*)) (rowScore <$> inp) (transpose $ rowScore <$> transpose inp)

solve' :: String -> Integer
solve' s = case parse input "" s of
  Left _ -> 0
  Right mat -> maximum $ concat $ score mat