-- Project: Decision tree
-- Author: Marián Tarageľ (xtarag01)
-- Year: 2025

import System.Environment

main :: IO ()
main = do
    (task : args) <- getArgs
    --let (Just action) = lookup task dispatch
    --action args
    if task == "-1"
    then task1  (args !! 0) (args !! 1)
    else task2 (args !! 0)


task1 :: String -> String -> IO ()
task1 tfile nfile = putStrLn "Úloha 1"

task2 :: String -> IO ()
task2 _ = putStrLn "Úloha 2"