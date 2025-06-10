---------- day 6 ----------

-- practice 1
getPosition :: Int -> [Int] -> Int
getPosition 1 (x : xs) = x
getPosition n (x : xs) = getPosition (n - 1) xs

-- practice 2
takeN :: Int -> [Int] -> [Int]
takeN 0 l = []
takeN 1 (x : xs) = [x]
takeN n (x : xs) = x : takeN (n - 1) xs

-- practice 3
dropN :: Int -> [Int] -> [Int]
dropN 0 l = l
dropN 1 (x : xs) = xs
dropN n (x : xs) = dropN (n - 1) xs

-- practice 4
listSize :: [Int] -> Int
listSize [] = 0
listSize (a : x) = 1 + listSize x

sumList :: [Int] -> Int
sumList [] = 0
sumList (a : x) = a + sumList x

averageList :: [Int] -> Int
averageList l = (sumList l) `div` (listSize l)

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

takeLarger :: Int -> [Int] -> [Int]
takeLarger 0 l = []
takeLarger n l = takeN n (reverse (quickSort l))

-- practice 6
countLarger :: Int -> [Int] -> Int
countLarger n l = listSize (larger n l)

-- practice 7
interleave :: [Int] -> [Int] -> [Int]
interleave [] [] = []
interleave l [] = l
interleave [] l = l
interleave (l1 : l1_tail) (l2 : l2_tail) = l1 : l2 : interleave l1_tail l2_tail

-- practice 8
duplicate :: [Int] -> [Int]
duplicate [] = []
duplicate (x : xs) = x : x : duplicate xs

-- practice 9
replicateChar :: Int -> Char -> [Char]
replicateChar 1 c = [c]
replicateChar n c = c : replicateChar (n - 1) c

replicateList :: Int -> [Char] -> [Char]
replicateList n [] = []
replicateList n (x : xs) = replicateChar n x ++ replicateList n xs

-- practice 10
dropCharHelper :: Int -> Int -> [Char] -> [Char]
dropCharHelper n count [] = []
dropCharHelper n count (x : xs)
    | count `mod` n == 0 = dropCharHelper n (count + 1) xs
    | otherwise = x : dropCharHelper n (count + 1) xs

dropEvery :: Int -> [Char] -> [Char]
dropEvery n s = dropCharHelper n 1 s

-- practice 11
takeChar :: Int -> [Char] -> [Char]
takeChar 0 l = []
takeChar 1 (x : xs) = [x]
takeChar n (x : xs) = x : takeChar (n - 1) xs

dropChar :: Int -> [Char] -> [Char]
dropChar 0 l = l
dropChar 1 (x : xs) = xs
dropChar n (x : xs) = dropChar (n - 1) xs

split :: Int -> [Char] -> ([Char], [Char])
split n s = (takeChar n s, dropChar n s)

-- practice 12
type Date = (Int, Int, Int)

-- practice 13
type Book = (String, String, String)
type Person = (String, String)
type Loan = (Person, Book, Date)

-- practice 14
data Library = Foundation | Private