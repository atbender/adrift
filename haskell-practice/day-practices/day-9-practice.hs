---------- day 9 ----------

-- practice 1
head2 :: [t] -> t
head2 (x : xs) = x

tail2 :: [t] -> [t]
tail2 (x : xs) = xs

fst2 :: (t1, t2) -> t1
fst2 (a, b) = a

shift2 :: ((t1, t2), t3) -> (t1, (t2, t3))
shift2 ((a, b), c) = (a, (b, c))

-- practice 2
concatenate :: [[t]] -> [t]
concatenate [] = []
concatenate (x : xs) = x ++ concatenate xs

-- practice 3
reverse' :: [t] -> [t]
reverse' [] = []
reverse' (x : xs) = reverse' xs ++ [x]

-- practice 4
zip3' :: [t1] -> [t2] -> [t3] -> [(t1, t2, t3)]
zip3' [] x y = []
zip3' x [] y = []
zip3' x y [] = []
zip3' (x : xs) (y : ys) (z : zs) = (x, y, z) : zip3' xs ys zs

-- practice 5
-- => is a type constraint, indicating that t1 can only be an instance of Num (due to +1)
mapPlusOne :: Num t1 => (t1 -> t2) -> [t1] -> [t2]
mapPlusOne f [] = []
mapPlusOne f (x : xs) = f (x + 1) : mapPlusOne f xs

-- practice 6
foldr2 :: (a -> b -> b) -> b -> [a] -> b
foldr2 f v [] = v
foldr2 f v (x : xs) = f x (foldr2 f v xs)

-- The function f (a -> b -> b) has the type of its first argument equal to the type of the list (x : xs) ([a]). Its second
-- argument needs to be of the same type as the result of this function, in order to apply it multiple times.

-- The argument v determines the output in case of an empty list, so it needs to share its type with the output.

-- example of foldr with multiple types:
-- foldr incOrDec 665 [True, True, False, False, True]
-- result = 666

-- example function used:
-- increments if Bool is True
-- decrements if False
incOrDec :: Bool -> Int -> Int
incOrDec True x = x + 1
incOrDec False x = x - 1