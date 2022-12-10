module Solution where

import Text.Parsec
import Text.Parsec.String (Parser)
import Control.Applicative (some)

import Utils (num)

data Instruction = Noop | Addx Int

instr :: Parser Instruction
instr = Noop <$ (string "noop" >> newline) <|>
  Addx <$> (string "addx" >> space >> num <* newline)

step :: Int -> Instruction -> (Int, [Int])
step x Noop = (x, [x])
step x (Addx v) = let x' = x + v in (x', [x, x'])

sim :: Int -> [Instruction] -> [Int]
sim _ [] = []
sim x (i:is) = do
  let (x', l) = step x i in l ++ sim x' is

contains :: Eq a => a -> [a] -> Bool
contains _ [] = False
contains e (x:xs)
  | x == e = True
  | otherwise = contains e xs

sig :: [Int] -> [Int]
sig l = do
  let c = map (subtract 1) [20, 60, 100, 140, 180, 220]
  [x * (i + 1) | (x, i) <- zip l [1..], contains i c]

solve :: String -> Int
solve input = do
  case parse (some instr <* eof) "" input of
    Left _ -> error "something went wrong"
    Right ins -> do
      let s = sim 1 ins
      sum $ sig s
