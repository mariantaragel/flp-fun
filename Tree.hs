-- Project: Decision tree
-- Author: Marián Tarageľ (xtarag01)
-- Year: 2025

module Tree
( buildTree
, parseEntry
, findClasses
, parseTrainData
, createTree
) where

import Data.List.Split (splitOn)
import Data.List (sort, nub, minimumBy)
import Data.Function (on)

data DecisionTree a b =
    EmptyTree |
    Leaf b    |
    Node a (DecisionTree a b) (DecisionTree a b)
    deriving (Eq, Read)

data TreeSide = LeftTree | RightTree
    deriving (Eq, Ord, Show, Read, Bounded, Enum)

data GiniIndex = GiniIndex { gini :: Float
                           , threshold :: Float
                           , index :: Int
                           } deriving (Show)

data TrainData = TrainData { values :: [Float]
                           , category :: String
                           } deriving (Show)

instance (Show a, Show b) => Show (DecisionTree a b) where
    showsPrec _ EmptyTree = showString "EmptyTree"
    showsPrec _ (Leaf x) = showString "Leaf: " . shows x
    showsPrec p (Node a l r) =
        showString "Node: " . shows a . showString "\n" . showTree (p + 1) l . showString "\n" . showTree (p + 1) r
        where
        showTree i EmptyTree = showString (indent i) . showString "EmptyTree"
        showTree i (Leaf x) = showString (indent i) . showString "Leaf: " . shows x
        showTree i (Node val left right) = showString (indent i) . showsPrec i (Node val left right)
        indent n = replicate (n * 2) ' '

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
    | value <= splitScore = findClass left entry
    | value > splitScore = findClass right entry
    where
    splitScore = snd x
    value = entry !! (fst x)
findClass _ _ = []

findClasses :: (Ord a) => DecisionTree (Int, a) [b] -> [[a]] -> [[b]]
findClasses _ [] = []
findClasses tree (x : xs) = (findClass tree x) : findClasses tree xs

getClass :: String -> String
getClass [] = []
getClass s = last $ splitOn "," s

getValues :: String -> [Float]
getValues [] = []
getValues s = map read (init $ splitOn "," s) :: [Float]

parseTrainData :: String -> TrainData
parseTrainData [] = TrainData [] ""
parseTrainData xs = TrainData (getValues xs) (getClass xs)

getFeature :: [TrainData] -> Int -> [Float]
getFeature [] _ = []
getFeature (x : xs) n = 
    if n < (length $ values x) 
    then ((values x) !! n) : getFeature xs n
    else error "error: index out of list"

findMidpoints :: [Float] -> [Float]
findMidpoints [] = []
findMidpoints (x : y : ys) = (x + y) / 2 : (findMidpoints (y:ys))
findMidpoints (_ : _) = []

findMinGini :: [GiniIndex] -> (Int, Float)
findMinGini xs = (index minGini, threshold minGini)
    where
    minGini = minimumBy (compare `on` gini) xs

createTree :: [TrainData] -> DecisionTree (Int, Float) String
createTree [] = EmptyTree
createTree tdata@(x : xs)
    | all (== (category x)) $ map category xs = Leaf (category x)
    | otherwise = Node splitScore (createTree leftValues) (createTree rightValues)
    where
    splitScore = findMinGini $ calcAllGinis tdata 0
    leftValues = [a | a <- tdata, ((values a) !! (fst splitScore)) <= (snd splitScore)]
    rightValues = [a | a <- tdata, ((values a) !! (fst splitScore)) > (snd splitScore)]

calcAllGinis :: [TrainData] -> Int -> [GiniIndex]
calcAllGinis [] _  = []
calcAllGinis tdata@(x : _) n
    | n < numFeatures = (calcFeatureGinis (zip vals categories) uniqClasses midpoints n) ++ calcAllGinis tdata (n + 1)
    | otherwise = []
    where
    vals = getFeature tdata n
    categories = map category tdata
    uniqClasses = nub $ categories
    midpoints = findMidpoints $ sort vals
    numFeatures = length $ values x

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
countClass _ _ = 0