module Utils where

import Data.List(transpose)

-- For convenience
type Vec a = [a]

zero :: (Num a, Eq a) => Vec a -> Bool
zero v = and $ (== 0) <$> v

neg :: Num a => Vec a -> Vec a
neg v = negate <$> v

add :: Num a => Vec a -> Vec a -> Vec a
add p q = zipWith (+) p q

sub :: Num a => Vec a -> Vec a -> Vec a
sub p q = add p (neg q)

-- Discrete integration
-- Given a_0 and Δa_0, Δa_1, ..., Δa_n, calculate a_0, a_1, ..., a_n
discInt :: Vec Int -> [Vec Int] -> [Vec Int]
discInt a [] = [a]
discInt a (d:ds) = a : discInt (add a d) ds

-- Discrete differentiation
-- Given a_0, a_1, .. a_n generate Δa_0, Δa_1, ..., Δa_{n - 1}
discDiff :: [Vec Int] -> [Vec Int]
discDiff v = zipWith sub v (tail v)

update :: Int -> a -> [a] -> Maybe [a]
update i e l
  | length l <= i = Nothing
  | otherwise = do
    let before = take i l
    let after = drop (i + 1) l
    Just $ before ++ [e] ++ after

place :: Vec Int -> a -> [[a]] -> Maybe [[a]]
place p e mat = do
  let (r, c) = (p !! 1, p !! 0)
  let row = mat !! r
  row' <- update c e row
  update r row' mat

flipRows :: [[a]] -> [[a]]
flipRows mat = transpose $ reverse <$> transpose mat

visualize :: Int -> Int -> Vec Int -> Vec Int -> String
visualize width height h t = do -- h for head position, t for tail position
  let board = replicate height (replicate width '.')
  case place t 'T' board of 
    Nothing -> error "Program should not reach this point"
    Just board' -> case place h 'H' board' of
      Nothing -> error "Program should not reach this point"
      Just board'' -> concat $ (++"\n") <$> flipRows board''