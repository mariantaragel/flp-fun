data DecisionTree a b =
    EmptyTree |
    Leaf b    |
    Node a (DecisionTree a b) (DecisionTree a b)
    deriving (Eq, Show, Read)

data TreeSide = Root | LeftTree | RightTree
    deriving (Eq, Ord, Show, Read, Bounded, Enum)

createNode :: a -> DecisionTree a b
createNode x = Node x EmptyTree EmptyTree

createLeaf :: b -> DecisionTree a b
createLeaf x = Leaf x

insertIntoTree :: (Eq a1, Eq a2, Eq b) => DecisionTree (a1, a2) b -> DecisionTree (a1, a2) b -> Bool -> a1 -> DecisionTree (a1, a2) b
insertIntoTree EmptyTree _ _ _ = EmptyTree
insertIntoTree (Leaf b) _ _ _ = EmptyTree
insertIntoTree (Node x left right) tree b n
    | (fst x) == n = if b then (Node x left tree) else (Node x tree right)
    | (fst x) /= n = Node x (insertIntoTree left tree b n) (insertIntoTree right tree b n)

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
    if (take 4 (dropSpaces s)) == "Node"
    then createNode (read ('(' : (drop 6 (dropSpaces s)) ++ ")") :: (Int, Float))
    else createLeaf (drop 6 (dropSpaces s))

leftRight :: [Int] -> [TreeSide]
leftRight [] = []
leftRight (x : xs) = [Root] ++ leftRight' xs [2 .. (maximum xs)]

leftRight' [] first = []
leftRight' (x : xs) first =
    if x `elem` first
    then [LeftTree] ++ leftRight' xs (deleteElem x first)
    else [RightTree] ++ leftRight' xs (deleteElem x first)

deleteElem :: (Eq a) => a -> [a] -> [a]
deleteElem _ [] = []
deleteElem n (x : xs)
    | x == n = deleteElem n xs
    | x /= n = x : (deleteElem n xs)
