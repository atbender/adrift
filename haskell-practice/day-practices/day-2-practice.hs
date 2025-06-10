---------- day 2 ----------

-- practice 1
maxi :: Int -> Int -> Int
maxi a b
    | a >= b     = a
    | otherwise  = b

-- practice 2
sales :: Int -> Int
sales 0 = 0
sales 1 = 4
sales 2 = 3
sales 7 = 0
sales _ = 10

maxSale :: Int -> Int
maxSale 0 = sales 0
maxSale n = maxi (sales n) (maxSale (n - 1))

-- practice 3
maxSaleWeek :: Int -> Int
maxSaleWeek n
    | sales n == maxSale n = n
    | otherwise = maxSaleWeek (n - 1)

-- practice 4
zeroSales :: Int -> Int
zeroSales n
    | n == -1 = -1
    | sales n == 0 = n
    | otherwise = zeroSales (n - 1)

-- practice 5
findWeek :: Int -> Int -> Int
findWeek s n
    | n == -1 = -1
    | sales n == s = n
    | otherwise = findWeek s (n - 1)

-- practice 6
findWeekWithZeroSales :: Int -> Int
findWeekWithZeroSales n = findWeek 0 n

-- practice 7
-- practice 7.1
maxSaleMN :: Int -> Int -> Int
maxSaleMN m n
    | m == n = sales n
    | otherwise = maxi (sales n) (maxSaleMN m (n - 1))

-- practice 7.2
maxSaleWeekMN :: Int -> Int -> Int
maxSaleWeekMN m n
    | sales n == maxSaleMN m n = n
    | otherwise = maxSaleWeekMN m (n - 1)

-- practice 7.3
zeroSalesMN :: Int -> Int -> Int
zeroSalesMN m n
    | m > n = -1
    | sales n == 0 = n
    | otherwise = zeroSalesMN m (n - 1)

-- practice 7.4
findWeekMN :: Int -> Int -> Int -> Int
findWeekMN s m n
    | m > n = -1
    | sales n == s = n
    | otherwise = findWeekMN s m (n - 1)

-- practice 7.5
findWeekWithZeroSalesMN :: Int -> Int -> Int
findWeekWithZeroSalesMN m n = findWeekMN 0 m n

-- practice 8
factorial :: Int -> Int
factorial 0 = 1
factorial x = x * factorial (x - 1)

-- practice 9
productMN :: Int -> Int -> Int
productMN m n
    | m == n = m
    | otherwise = m * productMN (m+1) n

-- practice 10
fib :: Int-> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)