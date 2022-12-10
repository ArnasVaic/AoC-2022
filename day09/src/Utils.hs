module Utils(Vec, add, sub) where

type Vec a = [a]

add :: Num a => Vec a -> Vec a -> Vec a
add p q = zipWith (+) p q

sub :: Num a => Vec a -> Vec a -> Vec a
sub p q = add p (negate <$> q)