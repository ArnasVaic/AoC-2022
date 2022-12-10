module Solution where

import Data.List
import Data.List.Split
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

solve' :: String -> String
solve' input = do
  case parse (some instr <* eof) "" input of
    Left _ -> error "something went wrong"
    Right ins -> do
      let s = sim 1 ins
      -- since CRT runs first, offset X value list by one
      let rows = chunksOf 40 (simCRT 0 (1 : init s))
      intercalate "\n" rows

-- part 2
pixel :: Int -> Int -> Char
pixel x p = if contains p [x - 1, x, x + 1] then '#' else '.'

simCRT :: Int -> [Int] -> [Char]
simCRT _ [] = []
simCRT iter (x:xs) = pixel x iter : simCRT (mod (iter + 1) 40) xs