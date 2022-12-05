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

crate :: Parser Char
crate = (char '[' *> satisfy isAlpha <* char ']') <|> (' ' <$ string "   ")

line :: Parser String
line = sepBy1 crate (char ' ') <* char '\n'

num :: Parser Int
num = read <$> some digit

instr :: Parser (Int, Int, Int)
instr = do
  amnt <- string "move " *> num <* char ' '
  orig <- string "from " *> num <* char ' '
  dest <- string "to " *> num <* char '\n'
  pure (amnt, orig, dest)

cols :: Parser ()
cols = () <$ (sepBy1 (char ' ' *> digit <* char ' ') (char ' ') >> string "\n\n")

-- 3-tuple order: amount, origin, destination
file :: Parser String
file = do
  -- TODO: avoid hardcoding height of block pile
  ls <- replicateM 8 line <* cols 
  ins <- some instr
  -- sequence of crate columns
  let columns = fromList $ trim <$> transpose ls
  -- simulate crane
  let result = foldl apply columns ins
  -- collect top crates
  pure $ toList $ head <$> result

-- simulate crane action
apply :: Seq String -> (Int, Int, Int) -> Seq String
apply xs (amm, origId, destId) = do
  let orig = index xs (origId - 1)
  let dest = index xs (destId - 1)
  let orig' = drop amm orig
  let dest' = reverse (take amm orig) ++ dest
  update (destId - 1) dest' $ update (origId - 1) orig' xs

solve :: String -> Either ParseError String
solve = parse file ""