module Lib where

import Data.Char
import Data.List
import Control.Applicative ( some )
import Text.Parsec
import Text.Parsec.String ( Parser )

priority :: Char -> Int
priority c
    | isLower c = ord c - 96
    | isUpper c = ord c - 38
    | otherwise = undefined

half :: String -> (String, String)
half s = splitAt (length s `div` 2) s

foo :: (String, String) -> Int
foo (x, y) = sum $ map priority $ nub $ x `intersect` y

line :: Parser Int
line = foo . half <$> some (satisfy isAlpha)

solve :: String -> Either ParseError Int
solve s = sum <$> parse (some (line <* char '\n')) "" s