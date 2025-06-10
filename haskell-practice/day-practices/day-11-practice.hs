---------- day 11 ----------

-- practice 1
data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
    deriving(Eq, Show)

-- practice 2
isWeekend :: Day -> Bool
isWeekend d = d == Saturday || d == Sunday

-- practice 3
data MaybeFloat = FloatValue Float | FloatError String
    deriving(Eq, Show)

-- practice 4
division :: Float -> Float -> MaybeFloat
division n1 n2
    | n2 == 0 = FloatError "Division by 0!"
    | otherwise = FloatValue (n1 / n2)

-- practice 5
data Nat = Zero | Succ Nat
    deriving(Eq, Show)

two :: Nat
two = Succ (Succ Zero)

seven :: Nat
seven = Succ (Succ (Succ (Succ (Succ (Succ (Succ Zero))))))

natToInt :: Nat -> Int
natToInt Zero = 0
natToInt (Succ nat) = succ (natToInt nat)

-- practice 6
intToNat :: Int -> Nat
intToNat 0 = Zero
intToNat n = Succ (intToNat (n - 1))