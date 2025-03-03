-- Project: Decision tree
-- Author: Marián Tarageľ (xtarag01)
-- Year: 2025

import System.IO
import Tree

main :: IO ()
main = do
    handle <- openFile "tree.txt" ReadMode
    contents <- hGetContents handle
    let xs = lines contents
    print xs
    let trees = map treeFromString xs
    print trees
    let parents = -1 : (findParent $ zip xs $ map countSpaces xs)
    print parents
    let lr = leftRight $ map countSpaces xs
    print lr
--    let inputs = makeInputs trees parents lr
--    print $ buildTree inputs
    hClose handle
