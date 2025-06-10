---------- day 10 ----------

-- practice 1
data Tree = Leaf Int | Node Int Tree Tree

-- practice 2
concatenate :: [[a]] -> [a]
concatenate l = foldr (++) [] l

-- practice 3
andList :: [Bool] -> Bool
andList l = foldr (&&) True l

-- practice 4
sumSquarePos :: [Int] -> Int
sumSquarePos l = foldr (+) 0 (map (^2) (filter (>0) l))

-- practice 5
sumLists :: [[Int]] -> Int
sumLists l = foldr (+) 0 (map (foldr (+) 0) l)

-- practice 6
sizeLists :: [[a]] -> Int
sizeLists l = foldr (+) 0 (map length l)

-- practice 7
reverse' :: [a] -> [a]
reverse' l = foldr (enqueue) [] l

enqueue :: a -> [a] -> [a]
enqueue n [] = [n]
enqueue n (x : xs) = x : enqueue n (xs)

-- practice 8
separateWords :: String -> [String]
separateWords [] = []
separateWords s = (takeWhile (/= ' ') s) : separateWords (dropWhile (== ' ') (dropWhile (/= ' ') s))

-- practice 9
data Tree2 a = Leaf2 a | Node2 a (Tree2 a) (Tree2 a)

-- practice 10
data List a = Nil | Cons a (List a)

-- practice 11
data Exp = Lit Int | Add Exp Exp | Sub Exp Exp