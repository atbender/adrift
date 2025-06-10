---------- day 3 ----------

-- practice 1
sumTuples :: ((Int, Int), (Int, Int)) -> Int
sumTuples ((a, b), (c, d)) = a + b + c + d

-- practice 2
shift :: ((Int, Int), Int) -> (Int, (Int, Int))
shift ((a, b), c) = (a, (b, c))

-- practice 3

maxi :: Int -> Int -> Int
maxi a b
    | a >= b     = a
    | otherwise  = b

mini :: Int -> Int -> Int
mini a b
    | a >= b    = b
    | otherwise = a


minAndMax :: Int -> Int -> Int -> (Int, Int)
minAndMax a b c = (maxi a (maxi b c), mini a (mini b c))

-- practice 4
sales :: Int -> Int
sales 0 = 1
sales 1 = 4
sales 2 = 3
sales 7 = 0
sales _ = 10

zeroSale :: Int -> (Int, Bool)
zeroSale n
    | n == -1 = (-1, False)
    | sales n == 0 = (n, True)
    | otherwise = zeroSale (n - 1)

-- practice 5
type Book = (String, String, String)

getTitle :: Book -> String
getTitle (title, author, isbn) = title

getAuthor :: Book -> String
getAuthor (title, author, isbn) = author

getIsbn :: Book -> String
getIsbn (title, author, isbn) = isbn

library :: Int -> Book
library 0 = ("House of Leaves", "Danielewski", "0-375-70376-4")
library _ = ("Concepts of Programming Languages", "Sebesta", "2-374-77421-4")

testBook :: String
testBook = "Title: " ++ getTitle (library 666) ++ "\nAuthor: " ++ getAuthor (library 666) ++ "\nISBN: " ++ getIsbn (library 666) ++ "\n"