---------- day 8 ----------

-- practice 1
applyTwice :: (Int -> Int) -> Int -> Int
applyTwice f x = f (f x)

increment :: Int -> Int
increment x = x + 1

square :: Int -> Int
square x = x * x

-- practice 2
totalSales :: (Int -> Int) -> Int -> Int
totalSales f 0 = f 0
totalSales f n = f n + totalSales f (n - 1)

sales :: Int -> Int
sales 0 = 0
sales 1 = 4
sales 2 = 3
sales 7 = 0
sales _ = 10

-- practice 3
foldInt :: (Int -> Int -> Int) -> [Int] -> Int
foldInt f [] = error "erro"
foldInt f [x] = x
foldInt f (x1 : x2 : xs) = f (f x1 x2) (foldInt f xs)

sum' :: Int -> Int -> Int
sum' x y = x + y

mult' :: Int -> Int -> Int
mult' x y = x * y

-- practice 4
filterString :: (Char -> Bool) -> [Char] -> [Char]
filterString f [] = []
filterString f (x : xs)
    | f x = x : filterString f xs
    | otherwise = filterString f xs

notSpace :: Char -> Bool
notSpace x = x /= ' '

-- practice 5
sumOfSquares :: [Int] -> Int
sumOfSquares [] = error "erro"
sumOfSquares l = foldInt sum' (mapInt (^2) l)

mapInt :: (Int -> Int) -> [Int] -> [Int]
mapInt f [] = []
mapInt f (x : xs) = f x : mapInt f xs

-- practice 6
iter :: Int -> (Int -> Int) -> Int -> Int
iter 1 f x = f x
iter n f x = f (iter (n - 1) f x)

-- practice 7
isLeap :: Int -> Bool
isLeap year = (year `mod` 4 == 0 && year `mod` 100 /= 0) || year `mod` 400 == 0

-- practice 8
type Date = (Int, Int, Int)

-- practice 9
daysInMonth :: Int -> Int -> Int
daysInMonth month year
    | month == 2 && isLeap year = 29
    | month == 2 = 28
    | month `elem` [4, 6, 9, 11] = 30
    | otherwise = 31

-- practice 10
type Book = (String, String, String)
type Person = (String, String)
type Loan = (Person, Book, Date)

data Library = Foundation | Private

-- practice 11
type Formula = (String, String)