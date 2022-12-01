module Lib where

import Data.List
import Control.Monad
import Control.Applicative ( some )

import Data.Foldable
import Text.Parsec
import Text.Parsec.String ( Parser )

parse_ :: Parser a -> String -> Either ParseError a
parse_ p = parse p "" 

num :: Parser Integer
num = read <$> some (oneOf ['0'..'9'])

elf :: Parser Integer
elf = do 
  xs <- some (num <* char '\n')
  pure $ sum xs

elves :: Parser [Integer]
elves = sepBy1 elf (char '\n')

-- solution for first part
solve :: String -> Either ParseError Integer
solve s = sum . (take 1) . reverse . sort<$> parse_ elves s

-- solution for part 2
solve' :: String -> Either ParseError Integer
solve' s = sum . (take 3) . reverse . sort <$> parse_ elves s