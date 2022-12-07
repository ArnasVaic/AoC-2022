module Lib2 where

import Data.List
import Text.Parsec
import Text.Parsec.String ( Parser )
import Control.Applicative (some)
import Control.Monad.State

update :: Int -> a -> [a] -> Maybe [a]
update i e l
  | length l > i = Nothing
  | otherwise = do
    let before = take i l
    let after = drop (i + 1) l
    Just $ before ++ [e] ++ after

num :: Parser Integer
num = read <$> some digit

itemName :: Parser String
itemName = some (letter <|> oneOf "./")

data Item = IFile Integer String | IFolder String deriving Show

item :: Parser Item
item = IFolder <$> (string "dir " *> itemName <* newline)<|> do
  size <- num <* space
  name <- itemName <* newline
  pure $ IFile size name

data Command = Back | To String | LS [Item] deriving Show

command :: Parser Command
command = 
  try (LS <$> (string "$ ls\n" *> some item)) <|>
  try (Back <$ (string "$ cd .." <* newline)) <|>
  To <$> (string "$ cd " *> itemName <* newline)

input :: Parser [Command]
input = some command

data FileTree = File Integer String | Folder Integer String [FileTree] deriving Show

fileSize :: FileTree -> Integer
fileSize (File s _) = s
fileSize (Folder s _ _) = s

folderName :: String -> FileTree -> Bool
folderName n (Folder _ m _) = n == m  
folderName _ File{} = False

-- 0 path
-- 1 file to post
-- 2 destination tree
createItem :: [String] -> FileTree -> FileTree -> Either String FileTree

-- Create the file
createItem _ _ File{} = Left "Cannot create a file in a file"
createItem [] f (Folder size name items) = Right $ Folder (fileSize f + size) name (f:items)

-- Go deeper until path is empty
-- 1) we look for path p in this folder
-- 2) create file in it
-- 3) 
createItem (p:ps) f (Folder size name items) = do
  case findIndex (folderName p) items of
    Nothing -> Left "Invalid path"
    Just i -> do
      let folder = items !! i
      folder' <- createItem ps f folder
      let size' = fileSize f + size 
      case update i folder' items of
        Nothing -> Left "Could not update"
        Just items' -> Right $ Folder size' name items'

solve :: String -> Either ParseError [Command]
solve = parse input ""