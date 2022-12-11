module Parser where

import Text.Parsec
import Text.Parsec.String (Parser)
import Control.Applicative (some)

data Monkey = Monkey {
  index :: Int,
  oper :: Int -> Int,
  items :: [Int],
  next :: Int -> Int
}

instance Show Monkey where
  show (Monkey index _ items _) = "Monkey " <> show index <> " " <> show items

numP :: Parser Int
numP = read <$> some digit

operationP :: Parser (Int -> Int)
operationP = do
  _ <- spaces >> string "Operation: new = old "
  operator <- (*) <$ char '*' <|> (+) <$ char '+'
  _ <- char ' '
  (operator <$> numP) <|> ((^2) <$ string "old")

testP :: Parser (Int -> Int)
testP = do
  _ <- spaces *> string "Test: divisible by "
  val <- numP <* newline
  _ <- spaces *> string "If true: throw to monkey "
  n <- numP <* newline
  _ <- spaces *> string "If false: throw to monkey "
  m <- numP <* newline
  pure $ \x -> if mod x val == 0 then n else m

itemsP :: Parser [Int]
itemsP = do
  _ <- spaces >> string "Starting items: "
  sepBy1 numP (string ", ") <* newline

monkey :: Parser Monkey
monkey = do
  index <- string "Monkey " *> numP <* char ':' <* newline
  items <- itemsP
  op <- operationP <* newline
  test <- testP
  pure $ Monkey index op items test