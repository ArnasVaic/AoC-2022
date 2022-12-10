module Visual where

import Data.List(transpose)

import Utils
import Solution

boardWidth :: Int
boardWidth = 6

boardHeight :: Int
boardHeight = 5

-- Discrete integration
-- Given a_0 and Δa_0, Δa_1, ..., Δa_n, calculate a_0, a_1, ..., a_n
discInt :: Vec Int -> [Vec Int] -> [Vec Int]
discInt a [] = [a]
discInt a (d:ds) = a : discInt (add a d) ds

-- Discrete differentiation
-- Given a_0, a_1, .. a_n generate Δa_0, Δa_1, ..., Δa_{n - 1}
discDiff :: [Vec Int] -> [Vec Int]
discDiff v = zipWith sub v (tail v)

-- Replace element in a list
update :: Int -> a -> [a] -> Maybe [a]
update i e l
  | length l <= i = Nothing
  | otherwise = do
    let before = take i l
    let after = drop (i + 1) l
    Just $ before ++ [e] ++ after

-- Replace element in a matrix
place :: Vec Int -> a -> [[a]] -> Maybe [[a]]
place p e mat = do
  let (r, c) = (p !! 1, p !! 0)
  let row = mat !! r
  row' <- update c e row
  update r row' mat

-- Replace many elements in a matrix
place' :: [(Vec Int, a)] -> [[a]] -> Maybe [[a]]
place' [] mat = Just mat
place' ((i,e):xs) mat = case place i e mat of
  Nothing -> Nothing
  Just mat' -> place' xs mat'

flipRows :: [[a]] -> [[a]]
flipRows mat = transpose $ reverse <$> transpose mat

vstep' :: [Vec Int] -> Vec Int -> IO ()
vstep' knots v = do
  putStrLn $ visualize knots
  let knots' = step' knots v
  putStrLn $ visualize knots'

visualize :: [Vec Int] -> String
visualize knots = do -- h for head position, t for tail position
  let board = replicate boardHeight (replicate boardWidth '.')
  let z' = zip knots "H123456789"
  case place' z' board of 
    Nothing -> error "Program should not reach this point"
    Just board' -> concat $ (++"\n") <$> flipRows board'