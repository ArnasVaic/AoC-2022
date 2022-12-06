module Lib where

import Data.List

subs :: Int -> String -> [String]
subs n s
    | length s >= n = take n s : subs n (tail s)
    | otherwise = []

solve :: String -> Int -> Maybe Int
solve s n = (+n) <$> findIndex ((== n) . length . nub) (subs n s)