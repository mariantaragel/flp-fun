data DecisionTree a b =
    EmptyTree |
    Leaf b    |
    Node a (DecisionTree a b) (DecisionTree a b)
    deriving (Eq, Show, Read)

data TreeSide = Root | LeftTree | RightTree
    deriving (Eq, Ord, Show, Read, Bounded, Enum)

data InputLine = InputLine { tree :: DecisionTree (Int, Float) String
                           , parent :: Int
                           , side :: TreeSide
                           } deriving (Show)

createNode :: a -> DecisionTree a b
createNode x = Node x EmptyTree EmptyTree

createLeaf :: b -> DecisionTree a b
createLeaf x = Leaf x

insertIntoTree :: DecisionTree (Int, Float) String -> InputLine -> DecisionTree (Int, Float) String
insertIntoTree EmptyTree input =
    if (side input) == Root
    then tree input
    else EmptyTree
insertIntoTree (Leaf b) _ = Leaf b
insertIntoTree (Node x left right) input
    | (fst x) == (parent input) = if (side input) == RightTree then (Node x left (tree input)) else (Node x (tree input) right)
    | (fst x) /= (parent input) = Node x (insertIntoTree left input) (insertIntoTree right input)

makeTuple :: String -> (Int, DecisionTree (Int, Float) String)
makeTuple s = (countSpaces s, treeFromString s)

countSpaces :: String -> Int
countSpaces "" = 0
countSpaces (x : xs)
    | x == ' ' = 1 + countSpaces xs
    | x /= ' ' = 0

dropSpaces :: String -> String
dropSpaces s = drop (countSpaces s) s

treeFromString :: String -> DecisionTree (Int, Float) String
treeFromString s =
    if (take 4 $ dropSpaces s) == "Node"
    then createNode $ parseNode s
    else createLeaf $ parseLeaf s

leftRight :: [Int] -> [TreeSide]
leftRight [] = []
leftRight (x : xs) = Root : (leftRight' xs [2 .. (maximum xs)])

leftRight' :: (Eq a) => [a] -> [a] -> [TreeSide]
leftRight' [] first = []
leftRight' (x : xs) first =
    if x `elem` first
    then LeftTree  : (leftRight' xs $ deleteElem x first)
    else RightTree : (leftRight' xs $ deleteElem x first)

deleteElem :: (Eq a) => a -> [a] -> [a]
deleteElem _ [] = []
deleteElem n (x : xs)
    | x == n = deleteElem n xs
    | x /= n = x : (deleteElem n xs)

findParent :: [(String, Int)] -> [Int]
findParent [] = []
findParent (x : y : z : xs) =
    if (snd y) == (snd z)
    then (fst $ parseNode $ (fst x)) : (fst $ parseNode $ (fst x)) : (findParent (z : xs)) 
    else (fst $ parseNode $ (fst x)) : (findParent (y : z : xs))
findParent (x : xs) = []

parseNode :: String -> (Int, Float)
parseNode s = read ('(' : (drop 6 (dropSpaces s)) ++ ")") :: (Int, Float)

parseLeaf :: String -> String
parseLeaf s = drop 6 $ dropSpaces s

makeInputs :: [DecisionTree (Int, Float) String] -> [Int] -> [TreeSide] -> [InputLine]
makeInputs [] [] [] = []
makeInputs (a : as) (b : bs) (c : cs) = (InputLine a b c) : (makeInputs as bs cs)

buildTree :: (Foldable a) => a InputLine -> DecisionTree (Int, Float) String
buildTree inputs = foldl insertIntoTree EmptyTree inputs