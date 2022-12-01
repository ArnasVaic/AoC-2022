module Lib where

import Data.List
import Control.Applicative ( some )
import Text.Parsec
import Text.Parsec.String ( Parser )

elf :: Parser Integer
elf = sum <$> some (read <$> some digit <* char '\n')

elves :: Parser [Integer]
elves = sepBy1 elf (char '\n')

solve :: Int -> String -> Either ParseError Integer
solve n s = sum . take n . reverse . sort <$> parse elves "" s