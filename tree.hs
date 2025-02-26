import System.IO

data Tree a b =
    Leaf b |
    Node a (Tree a b) (Tree a b)
    deriving (Eq, Show)

loadTree = do
    handle <- openFile "tree.txt" ReadMode
    contents <- hGetContents handle
    return $ length $ lines contents
    hClose handle
