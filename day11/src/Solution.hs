module Solution where

import Data.List
import Text.Parsec (parse, sepBy1, newline)
import Parser (Monkey(..), monkey)
import Utils (update)

-- simulate single monkey
-- go through all items
runMonkey :: Monkey -> ((Int, [Int]), (Int, [Int]))
runMonkey (Monkey _ op items test) = do
  let ids = decideItem op test <$> items
  let pairs = zip ((`div` 3) . op <$> items) ids
  -- this is disgusting
  let (id1, id2) = let ids' = nub ids in (head ids', if length ids' > 1 then ids' !! 1 else 0)
  ( (id1, fst <$> filter (\(_, i) -> i == id1) pairs),
    (id2, fst <$> filter (\(_, i) -> i /= id1) pairs))

updateMonkeys :: Monkey -> ((Int, [Int]), (Int, [Int])) -> [Monkey] -> [Monkey]
updateMonkeys (Monkey idx op _ test) ((id1, l1), (id2, l2)) monkeys = do
  let ms = update idx (Monkey idx op [] test) monkeys
  let (Monkey _ op1 items1 test1) = ms !! id1
  let ms' = update id1 (Monkey id1 op1 (items1 <> l1) test1) ms
  let (Monkey _ op2 items2 test2) = ms' !! id2
  update id2 (Monkey id2 op2 (items2 <> l2) test2) ms'

runRound :: [Monkey] -> [Monkey]
runRound [] = []
runRound monkeys = runRound' 0 monkeys where
  runRound' :: Int -> [Monkey] -> [Monkey]
  runRound' i ms
    | i == length ms = ms
    | otherwise = do
      let current = ms !! i
      let result = runMonkey current
      let ms' = updateMonkeys current result ms
      runRound' (i + 1) ms'

decideItem :: (Int -> Int) -> (Int -> Int) -> Int -> Int
-- wf - worry increase function
-- test - test function that returns the index of the monkey its gonna go to
decideItem wf test worry = test $ (wf worry) `div` 3

solve :: String -> [Monkey]
solve s = do
  case parse (sepBy1 monkey newline) "" s of
    Left _ -> []
    Right ms -> runRound ms