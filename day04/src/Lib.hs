{-# LANGUAGE TupleSections #-}
module Lib where
import Control.Applicative ( some )
import Text.Parsec
import Text.Parsec.String ( Parser )

type Range a = (a, a, a, a)

num :: Parser Int
num = read <$> some digit

ranges :: Parser (Range Int)
ranges = do
  a <- num <* char '-'
  b <- num <* char ','
  c <- num <* char '-'
  d <- num <* char '\n'
  return (a, b, c, d)

overlap :: Range Int -> Bool
overlap (a, b, c, d) = b >= c && d >= a

contains :: Range Int -> Bool
contains (a, b, c, d) = (a <= c && d <= b) || (c <= a && b <= d)

solve :: String -> (Range Int -> Bool) -> Either ParseError Int
solve s p = parse (length . filter (== True) <$> some (p <$> ranges)) "" s