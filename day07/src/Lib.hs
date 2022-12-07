module Lib where
{- 
import Data.Char ( isAlpha )
import qualified Data.List as List
import Data.Sequence as Sq
import Control.Monad.Trans.Class
import Text.Parsec
import Text.Parsec.String ( Parser )
import Control.Applicative ( some )

data File = File Integer String | Folder (Seq File) Integer String deriving Show

-- There can be three commands:
-- $ cd folder
-- $ cd ..
-- $ ls

-- Command always are of this form:
-- $ cd .. (this command could be repeated n times, n >= 0)
-- $ cd folder
-- $ ls

-- Abstraction of commands:
--  go n levels back (n >= 0)
--  folder name into which we go into
--  folder contents
data Directive = Directive Int File deriving Show

-- After we parse the directives we have to 'stich' together
-- the files into one tree

-- Natural number
num :: Parser Integer
num = read <$> some digit

-- Allowed file system names
title :: Parser String
title = some (letter <|> char '.' <|> char '/')

-- This isn't recursive, just a simple empty folder
folder :: Parser File
folder = do
  name <- string "dir " *> title <* newline
  pure $ Folder empty 0 name

-- This isn't recursive, just a simple file
file :: Parser File
file = do
  sz <- num <* space
  name <- title <* newline
  pure $ File sz name

directive :: Parser Directive
directive = do
  n <- many $ try (string "$ cd .." <* newline)
  name <- string "$ cd " *> title <* newline
  _ <- string "$ ls" >> newline
  contents <- some (file <|> folder)
  pure $ Directive (1 + List.length n) $ Folder (fromList contents) 0 name

inject :: Directive -> Directive -> Maybe Directive
inject p q = do
  let (Directive n (Folder items _ name)) = p
  let (Directive m (Folder target _ name2)) = q
  idx <- findIndexL (\f -> case f of
    File _ _ -> False
    Folder _ _ s -> s == name) target
  let target' = adjust' (\(Folder _ _ _) -> Folder items 0 name) idx target
  Just $ Directive m (Folder target' 0 name2)

-- pop one directive from the top and inject into a directive n layers up
stich :: Seq Directive -> Maybe Directive
stich ss
  | Sq.length ss == 1 = Just $ index ss 0
  | otherwise = do
  let d@(Directive n _) = index ss 0 
  let target = index ss n
  s' <- inject d target
  stich $ update (n - 1) s' $ Sq.drop 1 ss

solve :: String -> Either ParseError (Maybe Integer)
solve s = do
  r <- parse (List.reverse <$> some directive) "" s
  case stich $ fromList r of
    Nothing -> pure Nothing
    (Just (Directive _ tree)) -> do
      let fs = folderSizes $ calcSizes tree
      pure $ Just $ sum $ Sq.filter (<= 100000) fs
    --(Just (Directive _ tree)) -> pure $ Just $ calcSizes tree

solve' :: String -> Either ParseError [Directive]
solve' = parse (List.reverse <$> some directive) ""

size :: File -> Integer
size (File n _) = n
size (Folder _ n _) = n

calcSizes :: File -> File
calcSizes f@(File _ _) = f
calcSizes f@(Folder items _ name) = do
  let newItems = calcSizes <$> items
  let itemSizes = sum $ size <$> newItems
  Folder newItems itemSizes name

concatSq :: Seq (Seq a) -> Seq a
concatSq s
  | Sq.null s = empty
  | otherwise = do
    let h = Sq.index s 0
    let t = Sq.drop 1 s
    h >< concatSq t

folderSizes :: File -> Seq Integer
folderSizes (File _ _) = empty
folderSizes (Folder items sz _) = sz :<| concatSq (folderSizes <$> items) -}