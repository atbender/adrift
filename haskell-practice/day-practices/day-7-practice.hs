---------- day 7 ----------

-- practice 1
sumQuadruple :: [(Int, Int, Int, Int)] -> Int
sumQuadruple [] = 0
sumQuadruple ((a, b, c, d) : xs) = a + b + c + d + sumQuadruple xs

-- practice 2
sumTuples :: [((Int, Int), (Int, Int))] -> Int
sumTuples [] = 0
sumTuples (((a, b), (c, d)) : xs) = a + b + c + d + sumTuples xs

-- practice 3
zip' :: [Int] -> [Int] -> [(Int, Int)]
zip' [] l = []
zip' l [] = []
zip' (x : xs) (y : ys) = (x, y) : zip' xs ys

-- practice 4
zip3' :: [Int] -> [Int] -> [Int] -> [(Int, Int, Int)]
zip3' [] l1 l2 = []
zip3' l1 [] l2 = []
zip3' l1 l2 [] = []
zip3' (x : xs) (y : ys) (z : zs) = (x, y, z) : zip3' xs ys zs

-- practice 5
unzip' :: [(Int, Int)] -> ([Int], [Int])
unzip' [] = ([], [])
unzip' l = (unzipLeft l, unzipRight l)

unzipLeft :: [(Int, Int)] -> [Int]
unzipLeft [] = []
unzipLeft ((a, b) : xs) = a : unzipLeft xs

unzipRight :: [(Int, Int)] -> [Int]
unzipRight [] = []
unzipRight ((a, b) : xs) = b : unzipRight xs