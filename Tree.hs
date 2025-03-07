module Tree
( buildTree
) where

data DecisionTree a b =
    EmptyTree |
    Leaf b    |
    Node a (DecisionTree a b) (DecisionTree a b)
    deriving (Eq, Show, Read)

data TreeSide = LeftTree | RightTree
    deriving (Eq, Ord, Show, Read, Bounded, Enum)

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

parseEntry :: [String] -> [Double]
parseEntry [_] = []
parseEntry s = map read $ s :: [Double]

findClass :: (Ord a) => DecisionTree (Int, a) [b] -> [a] -> [b]
findClass _ [] = []
findClass (Leaf a) entry = a
findClass (Node x left right) entry
    | threshold > value = findClass left entry
    | threshold < value = findClass right entry
    where
    threshold = snd x
    value = entry !! (fst x)

