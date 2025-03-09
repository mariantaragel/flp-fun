-- Project: Decision tree
-- Author: Marián Tarageľ (xtarag01)
-- Year: 2025

module Tree
( buildTree
, parseEntry
, findClasses
) where

import Data.List.Split (splitOn)
import Data.List (sort, sortBy, nub)
import Data.Function (on)

data DecisionTree a b =
    EmptyTree |
    Leaf b    |
    Node a (DecisionTree a b) (DecisionTree a b)
    deriving (Eq, Show, Read)

data TreeSide = LeftTree | RightTree
    deriving (Eq, Ord, Show, Read, Bounded, Enum)

data GiniIndex = GiniIndex { gini :: Float
                           , threshold :: Float
                           , index :: Int
                           } deriving (Show)

createNode :: a -> DecisionTree a b
createNode x = Node x EmptyTree EmptyTree

createLeaf :: b -> DecisionTree a b
createLeaf x = Leaf x

insertIntoTree :: DecisionTree (Int, Float) String -> (DecisionTree (Int, Float) String, [TreeSide]) -> DecisionTree (Int, Float) String
insertIntoTree EmptyTree (tree, []) = tree
insertIntoTree (Node _ _ _) (tree, []) = tree
insertIntoTree (Node x left right) (tree, (y:ys))
    | y == LeftTree = Node x (insertIntoTree left (tree, ys)) right
    | y == RightTree = Node x left (insertIntoTree right (tree, ys))
insertIntoTree _ _ = EmptyTree

countSpaces :: (Num a) => String -> a
countSpaces (' ' : xs) = 1 + countSpaces xs
countSpaces _ = 0

dropSpaces :: String -> String
dropSpaces s = drop (countSpaces s) s

parseNode :: String -> (Int, Float)
parseNode s = read ('(' : (drop 6 (dropSpaces s)) ++ ")") :: (Int, Float)

parseLeaf :: String -> String
parseLeaf s = drop 6 $ dropSpaces s

treeFromString :: String -> DecisionTree (Int, Float) String
treeFromString s =
    if (take 4 $ dropSpaces s) == "Node"
    then createNode $ parseNode s
    else createLeaf $ parseLeaf s

findPaths :: [Int] -> [TreeSide] -> Int -> [[TreeSide]]
findPaths [] _ _ = []
findPaths (0 : xs) _ _ = [] : (findPaths xs [] 0)
findPaths (x : xs) path before
    | before == x = eqPath : (findPaths xs eqPath x)
    | before < x  = upPath : (findPaths xs upPath x)
    | before > x  = doPath : (findPaths xs doPath x)
    where
    eqPath = (init path) ++ [RightTree]
    upPath = path ++ [LeftTree]
    doPath = (take ((length path) - (before - x) - 1) path) ++ [RightTree]
findPaths _ _ _ = []

buildTree :: [String] -> DecisionTree (Int, Float) String
buildTree inputs = foldl insertIntoTree EmptyTree $ zip trees paths
    where
    trees = map treeFromString inputs
    paths = findPaths (map ((`div` 2) . countSpaces) inputs) [] 0

parseEntry :: String -> [Float]
parseEntry [] = []
parseEntry s = map read (splitOn "," s) :: [Float]

findClass :: (Ord a) => DecisionTree (Int, a) [b] -> [a] -> [b]
findClass _ [] = []
findClass (Leaf a) _ = a
findClass (Node x left right) entry
    | threshold > value = findClass left entry
    | threshold < value = findClass right entry
    where
    threshold = snd x
    value = entry !! (fst x)
findClass _ _ = []

findClasses :: (Ord a) => DecisionTree (Int, a) [b] -> [[a]] -> [[b]]
findClasses _ [] = []
findClasses tree (x : xs) = (findClass tree x) : findClasses tree xs

getClasses :: String -> String
getClasses [] = []
getClasses s = last $ splitOn "," s

getValues :: String -> [Float]
getValues [] = []
getValues s = map read (init $ splitOn "," s) :: [Float]

getFeature :: [[Float]] -> Int -> [Float]
getFeature [] _ = []
getFeature (x : xs) n = (x !! n) : getFeature xs n

findMidpoints :: [Float] -> [Float]
findMidpoints [] = []
findMidpoints (x : y : ys) = (x + y) / 2 : (findMidpoints (y:ys))
findMidpoints (x : ys) = []

sortFst :: Ord a => [(a, b)] -> [(a, b)]
sortFst xs = sortBy (compare `on` fst) xs

sortSnd :: Ord b => [(a, b)] -> [(a, b)]
sortSnd xs = sortBy (compare `on` snd) xs

findMin :: [[GiniIndex]] -> GiniIndex -> (Int, Float)
findMin [] m = (index m, threshold m)
findMin ((y : ys) : xs) m
    | gini y < gini m = findMin xs y
    | otherwise = findMin xs m

calcAllGinis :: [String] -> [[Float]] -> Int -> [[GiniIndex]]
calcAllGinis [] _ _ = []
calcAllGinis _ [] _ = []
calcAllGinis xs allValues@(y : ys) n
    | numFeatures > n = (sortBy (compare `on` gini) $ calcFeatureGinis (zip values xs) uniqClasses midpoints n) : calcAllGinis xs allValues (n + 1)
    | otherwise = []
    where
    values = getFeature allValues n
    uniqClasses = nub xs
    midpoints = findMidpoints $ sort values
    numFeatures = length y

calcFeatureGinis :: [(Float, String)] -> [String] -> [Float] -> Int -> [GiniIndex]
calcFeatureGinis _ _ [] _ = []
calcFeatureGinis xs c (y : ys) n = (GiniIndex (calcSplitGini xs c y) y n) : calcFeatureGinis xs c ys n

calcSplitGini :: [(Float, String)] -> [String] -> Float -> Float
calcSplitGini [] _ _ = 0.0
calcSplitGini xs c midpoint = l / t * (calcGini $ countClasses c left) + r / t * (calcGini $ countClasses c right)
    where
    left = [a | a <- xs, (fst a) <= midpoint]
    right = [a | a <- xs, (fst a) > midpoint]
    l = fromIntegral (length left) :: Float
    r = fromIntegral (length right) :: Float
    t = l + r

calcGini :: [Int] -> Float
calcGini [] = 0.0
calcGini xs = 1.0 - (sumClasses xs $ sum xs)

sumClasses :: [Int] -> Int -> Float
sumClasses [] _ = 0.0
sumClasses _ 0 = 1.0
sumClasses (x : xs) t = ((xf / tf) ** 2) + sumClasses xs t
    where
    xf = fromIntegral x :: Float
    tf = fromIntegral t :: Float

countClasses :: [String] -> [(Float, String)] -> [Int]
countClasses [] _ = []
countClasses (c : cs) d = countClass c d : (countClasses cs d)

countClass :: String -> [(Float, String)] -> Int
countClass _ [] = 0
countClass c (x : xs)
    | c == snd x = 1 + (countClass c xs)
    | c /= snd x = countClass c xs