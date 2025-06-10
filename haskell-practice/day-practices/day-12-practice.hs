---------- day 12 ----------

data Tree a = Leaf a | Node a (Tree a) (Tree a)
    deriving(Eq, Show)

-- practice 1
multiplyByTwo :: Tree Int -> Tree Int
multiplyByTwo (Leaf n) = Leaf (n * 2)
multiplyByTwo (Node n a1 a2) = Node (n * 2) (multiplyByTwo a1) (multiplyByTwo a2)

--       1
--    2     7
--  3   4

tree1 :: Tree Int
tree1 = Node 1 (Node 2 (Leaf 3) (Leaf 4)) (Leaf 7)

-- practice 2
countElements :: Tree Int -> Int
countElements (Leaf n) = 1
countElements (Node n a1 a2) = 1 + countElements a1 + countElements a2

-- practice 3
height :: Tree Int -> Int
height (Leaf n) = 1
height (Node n a1 a2) = 1 + max (height a1) (height a2)

-- practice 4
greatestElement :: Tree Int -> Int
greatestElement (Leaf n) = n
greatestElement (Node n a1 a2) = max (greatestElement a1) (greatestElement a2)

-- practice 5
searchInt :: Int -> Tree Int -> Bool
searchInt x (Leaf n) = n == x
searchInt x (Node n a1 a2) = (n == x) || (searchInt x a1) || (searchInt x a2)

-- practice 6
howManyTimes :: Int -> Tree Int -> Int
howManyTimes x (Leaf n)
    | n == x = 1
    | otherwise = 0

howManyTimes x (Node n a1 a2)
    | n == x = 1 + (howManyTimes x a1) + (howManyTimes x a2)
    | otherwise = (howManyTimes x a1) + (howManyTimes x a2)

-- practice 7
reflectTree :: Tree a -> Tree a
reflectTree (Leaf n) = (Leaf n)
reflectTree (Node n a1 a2) = (Node n (reflectTree a2) (reflectTree a1))

-- practice 8
treeToList :: Tree a -> [a]
treeToList (Leaf n) = [n]
treeToList (Node n a1 a2) = [n] ++ treeToList a1 ++ treeToList a2

-- practice 9
mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f (Leaf a) = Leaf (f a)
mapTree f (Node n a1 a2) = Node (f n) (mapTree f a1) (mapTree f a2)