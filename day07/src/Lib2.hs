module Lib2 where

import Data.List
import Text.Parsec
import Text.Parsec.String ( Parser )
import Control.Applicative (some)
import Control.Monad.State

update :: Int -> a -> [a] -> Maybe [a]
update i e l
  | length l <= i = Nothing
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

data Command = Back | Dir String [Item] deriving Show

command :: Parser Command
command = try (Back <$ (string "$ cd .." <* newline)) <|> do
  folder <- try (string "$ cd " *> itemName <* newline)
  items <- try (string "$ ls\n" *> some item) 
  pure $ Dir folder items

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
createItem _ _ File{} = Left "File cannot contain other files"
createItem [] _ _ = Left "No path"
createItem [folder] f (Folder size name items) = 
  if folder == name
  then Right $ Folder (fileSize f + size) name (f:items)
  else Left $ "Folder names don't match: " ++ name ++ ", " ++ folder

-- Go deeper until path is empty
createItem (p:ps) f (Folder size name items) = do
  if name /= p
    then Left "Invalid path"
    else case findIndex (folderName (head ps)) items of
      Nothing -> Left "Invalid path"
      Just i -> do
        let folder = items !! i
        folder' <- createItem ps f folder
        let size' = fileSize f + size 
        case update i folder' items of
          Nothing -> Left "Could not update"
          Just items' -> Right $ Folder size' name items'

createItems :: [String] -> [FileTree] -> FileTree -> Either String FileTree
createItems _ [] tree = Right tree
createItems path (i:is) tree = case createItem path i tree of 
  Left s -> Left s
  Right tree' -> createItems path is tree'

fromItem :: Item -> FileTree
fromItem (IFile size name) = File size name
fromItem (IFolder name) = Folder 0 name []


doCommand :: [String] -> FileTree -> Command -> Either String ([String], FileTree)

-- populate directory "name" with items
doCommand [] tree Back = Left "No more going back D:"
doCommand path tree Back = pure (init path, tree)
doCommand _ File{} Dir{} = Left "Cannot place a folder inside a file"
doCommand path tree (Dir name items) = do
  let files = fromItem <$> items
  let path' = path ++ [name]
  tree' <- createItems path' files tree
  pure (path', tree')

-- build a file tree
foo :: [String] -> FileTree -> [Command] -> Either String ([String], FileTree, [Command])
foo path tree [] = pure (path, tree, [])
foo path tree (c:cs) = do
  (path', tree') <- doCommand path tree c
  foo path' tree' cs 

solve :: String -> Either ParseError [Command]
solve = parse input ""