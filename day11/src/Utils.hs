module Utils where

import Parser

--import System.IO

update :: Int -> a -> [a] -> [a]
update i e l
  | 0 > i || i >= length l = l
  | otherwise = do
    let bf = take i l
    let af = drop (i + 1) l
    bf <> [e] <> af

mktest :: Int -> Int -> Int -> (Int -> Int)
mktest a b c = \x -> if mod x a == 0 then b else c