-- Project: Decision tree
-- Author: Marián Tarageľ (xtarag01)
-- Year: 2025

import System.Environment
import System.IO
import Tree

dispatch :: [(String, [String] -> IO ())]
dispatch = [ ("-1", task1)
           , ("-2", task2)
           ]

main :: IO ()
main = do
    (command:args) <- getArgs
    let (Just action) = lookup command dispatch
    action args

task1 :: [String] -> IO ()
task1 [] = error "error: No arguments"
task1 [treeFile, entryFile] = do
    treeContents <- readFile treeFile
    entryContents <- readFile entryFile
    let treeInputs = lines treeContents
    let entries = lines entryContents
    print $ buildTree treeInputs
    print entries
task1 _ = error "error: Wrong number of arguments"

task2 :: [String] -> IO ()
task2 [] = error "chybka"
task2 [fileName] = do
    handle <- openFile fileName ReadMode
    contents <- hGetContents handle
    let inputs = lines contents
    print $ buildTree inputs
    hClose handle
task2 _ = error "chybka"