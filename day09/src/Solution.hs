module Solution where

import Data.List (nub)
import Text.Parsec
import Text.Parsec.String (Parser)
import Control.Applicative (some)

import Utils

touch :: (Num a, Ord a) => Vec a -> Vec a -> Bool
touch p q = and $ (<=1) <$> abs <$> sub p q

-- logic table for new position
-- touch diagmove   p
--   1      1       t
--   1      0       t
--   0      1       t + v
--   0      0       h

-- Refactored step
step :: Vec Int -> Vec Int -> Vec Int -> Vec Int
step h v t = do
  let diagonal = and $ (>=1) . abs <$> v
  let touching = touch t (add h v)
  case (touching, diagonal) of
    (True, True)    -> t
    (True, False)   -> t
    (False, True)   -> add t v
    (False, False)  -> h

-- Simulate all steps
simulate :: Vec Int -> Vec Int -> [Vec Int] -> [Vec Int]
simulate _ _ [] = []
simulate h t (v:vs) = do -- h for head, t for tail
  let t' = step h v t
  t' : simulate (add h v) t' vs

sim :: [Vec Int] -> [Vec Int] -> [[Vec Int]]
sim knots [] = [knots]
sim knots (v:vs) = knots : sim (step' knots v) vs

dir :: Parser [Vec Int]
dir = do
  d <-  [-1,  0] <$ char 'L' <|> 
        [ 1,  0] <$ char 'R' <|> 
        [ 0,  1] <$ char 'U' <|> 
        [ 0, -1] <$ char 'D'
  n <- space *> (read <$> some digit) <* newline
  pure $ replicate n d

solve :: String -> Maybe Int
solve s = do
  case parse (some dir) "" s of
    Left _ -> Nothing
    Right directions -> do
      let dirs = concat directions
      let tails = [0,0] : simulate [0, 0] [0, 0] dirs
      Just $ length $ nub tails

-- part 2 :)

-- Given a list of knots (at time t!!!), and velocity of time t
-- of the head knot, calculate new knot positions at time t1
step' :: [Vec Int] -> Vec Int -> [Vec Int]
step' [] _ = [] -- empty rope doesn't move anywhere
step' [x] v = [add x v] -- one knot is just a solid object
step' (x:y:xs) vx = do
  let x' = add x vx       -- New head position
  let y' = step x vx y  -- Calculate new position of the next knot
  let vy = sub y' y       -- Calculate velocity of the next knot
  x' : step' (y:xs) vy