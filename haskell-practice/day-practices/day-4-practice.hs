---------- day 4 ----------

-- practice 1
multiplyEachByTwo :: [Int] -> [Int]
multiplyEachByTwo [] = []
multiplyEachByTwo (a : x) = (a * 2) : multiplyEachByTwo x

-- practice 2
listSize :: [Int] -> Int
listSize [] = 0
listSize (a : x) = 1 + listSize x

-- practice 3
productList :: [Int] -> Int
productList [] = 1
productList (a : x) = a * productList x

-- practice 4
andList :: [Bool] -> Bool
andList [] = True
andList (a : x) = a && andList x

-- practice 5
concatLists :: [[Int]] -> [Int]
concatLists [] = []
concatLists (a : x) = a ++ concatLists x

-- practice 6
reverseList :: [Int] -> [Int]
reverseList [] = []
reverseList (x : xs) = reverseList xs ++ [x]

-- practice 7
isMultipleOf3 :: Int -> Bool
isMultipleOf3 x = mod x 3 == 0

-- practice 8
sumPairs :: (Int, Int, Int, Int) -> Int
sumPairs (a, b, c, d) = a + b + c + d

-- practice 9
type IntTuple = (Int, Int, Int)

-- practice 10
-- ... existing code ...

-- practice 11
-- ... existing code ...

-- practice 12
-- ... existing code ...