module Visual where

import Data.List(transpose, intercalate)

import Utils
import Solution

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

traceTail :: [[Vec Int]] -> IO ()
traceTail ropes = do
  let tp = last <$> ropes
  putStrLn ""

showSim :: [Vec Int] -> [Vec Int] -> IO ()
showSim knots vs = do
  let knots' = sim knots vs
  let minX = minimum $ minimum $ map (!! 0) <$> knots'
  let minY = minimum $ minimum $ map (!! 1) <$> knots'
  let knots'' = map (add [-minX, -minY]) <$> knots'
  let maxX = maximum $ maximum $ map (!! 0) <$> knots''
  let maxY = maximum $ maximum $ map (!! 1) <$> knots''
  putStrLn $ intercalate "\n" $ visualize (maxX + 1) (maxY + 1) <$> knots''

visualize :: Int -> Int -> [Vec Int] -> String
visualize w h knots = do -- h for head position, t for tail position
  let board = replicate h (replicate w '.')
  let z' = zip knots "H123456789"
  case place' (reverse z') board of 
    Nothing -> error "Program should not reach this point"
    Just board' -> concat $ (++"\n") <$> flipRows board'