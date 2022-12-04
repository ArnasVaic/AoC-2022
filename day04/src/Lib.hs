module Lib where

import Data.Char
import Data.List ( intersect, nub )
import Control.Monad
import Control.Applicative ( some )
import Text.Parsec
import Text.Parsec.String ( Parser )

num :: Parser Int
num = read <$> some (satisfy isDigit)

pair :: Parser Bool
pair = do
  a0 <- num <* char '-'
  a1 <- num <* char ','
  b0 <- num <* char '-'
  b1 <- num <* char '\n'
  pure $ (a0 >= b0 && a1 <= b1) || (b0 >= a0 && b1 <= a1)

pair' :: Parser Bool
pair' = do
  a0 <- num <* char '-'
  a1 <- num <* char ','
  b0 <- num <* char '-'
  b1 <- num <* char '\n'
  pure $ not $ a1 < b0 || b1 < a0

solve :: String -> Parser Bool -> Either ParseError Int
solve s p = parse (length . filter (== True) <$> some p) "" s