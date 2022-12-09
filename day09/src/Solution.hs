module Solution where

import Data.List (nub)
import Text.Parsec
import Text.Parsec.String (Parser)
import Control.Applicative (some)

import Utils

touch :: (Num a, Ord a) => Vec a -> Vec a -> Bool
touch p q = and $ (<=1) <$> abs <$> sub p q

-- Single simulation step
step :: Vec Int -> Vec Int -> Vec Int -> Vec Int
step h h' t = if touch t h' then t else h

-- Simulate all steps
simulate :: Vec Int -> Vec Int -> [Vec Int] -> [Vec Int]
simulate _ _ [] = []
simulate h t (v:vs) = do -- h for head, t for tail
  let h' = add h v
  let t' = step h h' t
  t' : simulate h' t' vs

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