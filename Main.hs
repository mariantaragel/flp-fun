-- Project: Decision tree
-- Author: Marián Tarageľ (xtarag01)
-- Year: 2025

import System.Environment
import Tree

-- Based on arguments choose task1 / task2
dispatch :: [(String, [String] -> IO ())]
dispatch = [ ("-1", task1)
           , ("-2", task2)
           ]

-- Main function
main :: IO ()
main = do
    (command:args) <- getArgs
    case lookup command dispatch of Just action -> action args
                                    Nothing -> error "error: Argument not found"

-- Handle task 1
task1 :: [String] -> IO ()
task1 [] = error "error: No arguments"
task1 [treeFile, entryFile] = do
    treeContents <- readFile treeFile
    entryContents <- readFile entryFile
    let tree = buildTree $ lines treeContents
    let newData = map parseEntry $ lines entryContents
    putStr $ unlines $ findClasses tree newData
task1 _ = error "error: Wrong number of arguments"

-- Handle task 2
task2 :: [String] -> IO ()
task2 [] = error "error: No arguments"
task2 [fileName] = do
    contents <- readFile fileName
    let tree = createTree $ map parseTrainData $ lines contents
    print tree
task2 _ = error "error: Wrong number of arguments"