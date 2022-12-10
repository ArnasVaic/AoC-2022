module Utils where

import Text.Parsec
import Text.Parsec.String (Parser)
import Control.Applicative (some)

nat :: Parser Int
nat = read <$> some digit

num :: Parser Int
num = nat <|> (char '-' >> negate <$> nat)