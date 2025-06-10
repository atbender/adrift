---------- day 5 ----------

-- practice 1
isMember :: Int -> [Int] -> Bool
isMember n []       = False
isMember n (x : xs) = x == n || isMember n xs

-- practice 2
memberCount :: Int -> [Int] -> Int
memberCount n [] = 0
memberCount n (x : xs)
    | x == n = 1 + memberCount n xs
    | otherwise = memberCount n xs

-- practice 3
isMember2 :: Int -> [Int] -> Bool
isMember2 n [] = False
isMember2 n x
    | memberCount n x > 0 = True
    | otherwise = False

-- practice 4
unique :: [Int] -> [Int]
unique x = unique2 x x

unique2 :: [Int] -> [Int] -> [Int]
unique2 y [] = []
unique2 y (x : xs)
    | memberCount x y == 1 = x : unique2 y xs
    | otherwise = unique2 y xs

-- practice 5
quickSort :: [Int] -> [Int]
quickSort [] = []
quickSort (x : xs) =    quickSort (smaller x xs)
                        ++ [x] ++
                        quickSort (larger x xs)

smaller :: Int -> [Int] -> [Int]
smaller n [] = []
smaller n (x : xs)
    | n >= x = x : smaller n xs
    | otherwise = smaller n xs

larger :: Int -> [Int] -> [Int]
larger n [] = []
larger n (x : xs)
    | n < x = x : larger n xs
    | otherwise = larger n xs