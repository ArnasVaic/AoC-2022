{-# LANGUAGE TupleSections #-}
module Lib where

import Data.Char
import Data.Foldable
import Data.List ( transpose)
import Data.List.Extra ( trim )
import Data.Sequence hiding (replicateM, take, reverse, drop)
import Control.Monad
import Control.Applicative ( some )
import Text.Parsec
import Text.Parsec.String ( Parser )

-- (Amount, Origin, Destination)
type Instruction = (Int, Int, Int)

-- Parse a crate
crate :: Parser Char
crate = (char '[' *> satisfy isAlpha <* char ']') <|> (' ' <$ string "   ")

-- Parse multiple crates in one line
line :: Parser String
line = sepBy1 crate (char ' ') <* char '\n'

-- Parse natural number
num :: Parser Int
num = read <$> some digit

-- Parse instruction
instruction :: Parser Instruction
instruction = do
  amount <- string "move " *> num <* char ' '
  origin <- string "from " *> num <* char ' '
  destination <- string "to " *> num <* char '\n'
  pure (amount, origin, destination)

-- Parse the line that contains column numbers
cols :: Parser ()
cols = sepBy1 (char ' ' *> digit <* char ' ') (char ' ') >> () <$ string "\n\n"

-- Parse whole file
file :: Parser (Seq String, [Instruction])
file = do
  -- TODO: avoid hardcoding height of crate pile
  crates <- replicateM 8 line <* cols 
  instructions <- some instruction
  -- sequence of crate columns
  pure $ (fromList $ trim <$> transpose crates, instructions)

-- Simulate crane action
-- Bool parameter is required to simulate both crane versions 
-- (one reverses boxes and the other does not when moving multiple boxes)
simulate :: Bool -> Seq String -> (Int, Int, Int) -> Seq String
simulate rev xs (amm, origId, destId) = do
  let orig = index xs (origId - 1)
  let dest = index xs (destId - 1)
  let orig' = drop amm orig
  let dest' = (if rev then reverse (take amm orig) else take amm orig) ++ dest
  update (destId - 1) dest' $ update (origId - 1) orig' xs

-- Solve
solve :: String -> Bool -> Either ParseError String
solve s rev = do
  -- Parse
  (crates, instructions) <- parse file "" s
  -- Simulate
  let result = foldl (simulate rev) crates instructions
  -- Collect what's needed for the answer
  pure $ toList $ head <$> result