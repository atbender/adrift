---------- day 1 ----------

-- practice 1
areFourEqual :: Int -> Int -> Int -> Int -> Bool
areFourEqual a b c d = (a == b) && (b == c) && (c == d)

-- practice 2
howManyAreEqual :: Int -> Int -> Int -> Int
howManyAreEqual a b c
    | (a == b) && (b == c)                 = 3
    | (a == b) || (b == c) || (a == c)     = 2
    | otherwise                            = 0

-- practice 3
allDifferent :: Int -> Int -> Int -> Bool
allDifferent a b c = (a /= b) && (b /= c) && (a /= c)

-- practice 4
-- This definition does not check if n is different from p.
-- The /= operator is not transitive.
-- C.E: n = 2, m = 3, p = 2

-- practice 5
allEqual :: Int -> Int -> Int -> Bool
allEqual a b c = (a == b) && (b == c)


howManyAreEqualReuse :: Int -> Int -> Int -> Int
howManyAreEqualReuse a b c
    | allDifferent a b c = 0
    | allEqual a b c     = 3
    | otherwise          = 2

-- practice 6
powerOfTwo :: Int -> Int
powerOfTwo x = x * x

-- practice 7
powerOfFour :: Int -> Int
powerOfFour x = powerOfTwo x * powerOfTwo x

-- practice 8
sales :: Int -> Int
sales 0 = 0
sales 1 = 4
sales 2 = 3
sales 7 = 0
sales _ = 10

totalSales :: Int -> Int
totalSales 0 = sales 0
totalSales n = sales n + totalSales (n - 1)