-- Project: Decision tree
-- Author: Marián Tarageľ (xtarag01)
-- Year: 2025

import System.IO

main :: IO ()
main = do
    handle <- openFile "tree.txt" ReadMode
    contents <- hGetContents handle
    let x = lines contents
    print x
    hClose handle
