module Lib where

import Control.Applicative ( some )
import Text.Parsec
import Text.Parsec.String ( Parser )

data Hand = Rock | Paper | Scissors deriving (Eq, Show)

win :: Hand -> Hand
win Rock = Scissors
win Paper = Rock
win Scissors = Paper

lose :: Hand -> Hand
lose Rock = Paper
lose Paper = Scissors
lose Scissors = Rock

-- Rock Paper Scissors Rock Scissors
handScore :: Hand -> Int
handScore Rock = 1
handScore Paper = 2
handScore Scissors = 3

winScore :: Hand -> Hand -> Int
winScore x y
  | y == win x  = 6
  | y == lose x = 0
  | otherwise   = 3

score :: Hand -> Hand -> Int
score x y = handScore x + winScore x y

hand :: Parser Hand
hand =
      Rock      <$ oneOf "AX"
  <|> Paper     <$ oneOf "BY"
  <|> Scissors  <$ oneOf "CZ"

-- part 1
line :: Parser Int
line = do
  a <- hand <* char ' '
  b <- hand <* char '\n'
  pure $ score b a

choose :: Char -> Hand -> Hand
choose 'X' = win
choose 'Y' = id
choose 'Z' = lose
choose _ = undefined

-- part 2
line' :: Parser Int
line' = do
  a <- hand <* char ' '
  c <- oneOf "XYZ" <* char '\n'
  pure $ score (choose c a) a

solve :: String -> Parser Int -> Either ParseError Int
solve s p = parse (sum <$> some p) "" s