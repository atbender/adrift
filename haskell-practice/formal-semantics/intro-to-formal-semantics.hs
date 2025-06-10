-- This file contains a series of exercises and examples demonstrating fundamental concepts in
-- functional programming and formal semantics using Haskell. It starts with basic abstract
-- data types and progresses to more complex structures like expression trees.

----------
-- Part 1: Basic Abstract Data Types
----------

data Temperature = Cold | Hot
    deriving(Eq, Show)

data Season = Summer | Autumn | Winter | Spring
    deriving(Eq, Show)

weather :: Season -> Temperature
weather Summer = Hot
weather _      = Cold

data Shape = Circle Float | Rectangle Float Float | Triangle Float Float
    deriving(Eq, Show)

isRound :: Shape -> Bool
isRound (Circle _)     = True
isRound (Rectangle _ _) = False
isRound (Triangle _ _)  = False

area :: Shape -> Float
area (Circle r)      = pi * r * r
area (Rectangle b a) = b * a
area (Triangle b a)  = b * a / 2

----------
-- Part 2: A Simple Binary Tree
----------
-- The following data type represents a binary tree with integer values at each leaf and node.
-- This serves as a foundational example for tree-like structures, similar to what's used
-- to represent expressions in a language (Abstract Syntax Trees).

data IntTree = Leaf Int | Node Int IntTree IntTree
    deriving(Eq, Show)

-- Example Tree:
--       1
--      / \
--     2   7
--    / \
--   3   4
tree1 :: IntTree
tree1 = Node 1 (Node 2 (Leaf 3) (Leaf 4)) (Leaf 7)

sumTree :: IntTree -> Int
sumTree (Leaf n)        = n
sumTree (Node n t1 t2) = n + sumTree t1 + sumTree t2

-- Practice functions for the IntTree:

-- 1. Multiply every value in the tree by 2.
multiplyTree :: IntTree -> IntTree
multiplyTree (Leaf n)        = Leaf (n * 2)
multiplyTree (Node n t1 t2) = Node (2 * n) (multiplyTree t1) (multiplyTree t2)

-- 2. Count the total number of nodes and leaves in the tree.
countTreeNodes :: IntTree -> Int
countTreeNodes (Leaf _)      = 1
countTreeNodes (Node _ t1 t2) = 1 + countTreeNodes t1 + countTreeNodes t2

-- 3. Find the largest element in the tree.
maxElemTree :: IntTree -> Int
maxElemTree (Leaf n)        = n
maxElemTree (Node n t1 t2) = max n (max (maxElemTree t1) (maxElemTree t2))

-- 4. Check if a given integer occurs in the tree.
occursInTree :: IntTree -> Int -> Bool
occursInTree (Leaf n) x        = n == x
occursInTree (Node n t1 t2) x = (n == x) || (occursInTree t1 x) || (occursInTree t2 x)

-- 5. Count the number of occurrences of an integer in the tree.
countOccurrencesInTree :: IntTree -> Int -> Int
countOccurrencesInTree (Leaf n) x
    | n == x    = 1
    | otherwise = 0
countOccurrencesInTree (Node n t1 t2) x
    | n == x    = 1 + (countOccurrencesInTree t1 x) + (countOccurrencesInTree t2 x)
    | otherwise = (countOccurrencesInTree t1 x) + (countOccurrencesInTree t2 x)

-- 6. Reflect the tree (swap left and right children at every node).
reflectTree :: IntTree -> IntTree
reflectTree (Leaf n)        = Leaf n
reflectTree (Node n t1 t2) = Node n (reflectTree t2) (reflectTree t1)

-- 7. Calculate the height of the tree.
treeHeight :: IntTree -> Int
treeHeight (Leaf _)      = 1
treeHeight (Node _ t1 t2) = 1 + max (treeHeight t1) (treeHeight t2)

-- 8. Convert the tree to a list of its values (pre-order traversal).
treeToList :: IntTree -> [Int]
treeToList (Leaf n)        = [n]
treeToList (Node n t1 t2) = [n] ++ treeToList t1 ++ treeToList t2

-- 9. Apply a function to every element in the tree.
mapTree :: (Int -> Int) -> IntTree -> IntTree
mapTree f (Leaf a)       = Leaf (f a)
mapTree f (Node n t1 t2) = Node (f n) (mapTree f t1) (mapTree f t2)

----------
-- Part 3: Abstract Syntax Trees for a Simple Imperative Language
----------
-- In formal semantics, we represent programs as tree-like data structures called
-- Abstract Syntax Trees (ASTs). The following data types define the AST for a
-- simple language with arithmetic and boolean expressions, and commands.

-- The 'State' is a list of (Variable, Value) pairs, representing the memory.
type State = [(String, Int)]

-- Fetches a variable's value from the state.
lookupVar :: State -> String -> Int
lookupVar [] x = 0 -- Default to 0 if not found
lookupVar ((s, v):xs) x
    | s == x    = v
    | otherwise = lookupVar xs x

-- Arithmetic Expressions (AExp)
data AExp
    = ANum Int
    | AVar String
    | AAdd AExp AExp
    | ASub AExp AExp
    | AMul AExp AExp
    deriving (Eq, Read, Show)

-- Boolean Expressions (BExp)
data BExp
    = BTrue
    | BFalse
    | BNot BExp
    | BAnd BExp BExp
    | BOr BExp BExp
    | BEq AExp AExp
    deriving (Eq, Read, Show)

-- Commands (CExp)
data CExp
    = CWhile BExp CExp
    | CIf BExp CExp CExp
    | CSeq CExp CExp
    | CAssign String AExp -- Changed Atrib to use String for variable name
    | CSkip
    deriving (Eq, Read, Show)

----------
-- Part 4: Small-Step Operational Semantics
----------
-- Small-step semantics define how a program executes one step at a time.
-- The functions below define the evaluation rules for our simple language.

-- A single step of evaluation for an arithmetic expression.
aSmallStep :: (AExp, State) -> (AExp, State)
aSmallStep (AVar x, s) = (ANum (lookupVar s x), s)
aSmallStep (AAdd (ANum x) (ANum y), s) = (ANum (x + y), s)
aSmallStep (AAdd (ANum x) e2, s) = let (e2', s') = aSmallStep (e2, s) in (AAdd (ANum x) e2', s')
aSmallStep (AAdd e1 e2, s) = let (e1', s') = aSmallStep (e1, s) in (AAdd e1' e2, s')
aSmallStep (ASub (ANum x) (ANum y), s) = (ANum (x - y), s)
aSmallStep (ASub (ANum x) e2, s) = let (e2', s') = aSmallStep (e2, s) in (ASub (ANum x) e2', s')
aSmallStep (ASub e1 e2, s) = let (e1', s') = aSmallStep (e1, s) in (ASub e1' e2, s')
aSmallStep (AMul (ANum x) (ANum y), s) = (ANum (x * y), s)
aSmallStep (AMul (ANum x) e2, s) = let (e2', s') = aSmallStep (e2, s) in (AMul (ANum x) e2', s')
aSmallStep (AMul e1 e2, s) = let (e1', s') = aSmallStep (e1, s) in (AMul e1' e2, s')
aSmallStep (e, s) = (e, s) -- No rule applies, expression is in normal form or stuck

-- Check if an arithmetic expression is a final value.
isFinalA :: AExp -> Bool
isFinalA (ANum _) = True
isFinalA _        = False

-- Repeatedly apply aSmallStep until a final value is reached.
interpretA :: (AExp, State) -> (AExp, State)
interpretA (a, s) = if isFinalA a then (a, s) else interpretA (aSmallStep (a, s))

-- A single step of evaluation for a boolean expression.
bSmallStep :: (BExp, State) -> (BExp, State)
bSmallStep (BNot BFalse, s) = (BTrue, s)
bSmallStep (BNot BTrue, s) = (BFalse, s)
bSmallStep (BNot b, s) = let (b', s') = bSmallStep (b, s) in (BNot b', s')
bSmallStep (BAnd BTrue b2, s) = (b2, s)
bSmallStep (BAnd BFalse _, s) = (BFalse, s)
bSmallStep (BAnd b1 b2, s) = let (b1', s') = bSmallStep (b1, s) in (BAnd b1' b2, s')
bSmallStep (BOr BTrue _, s) = (BTrue, s)
bSmallStep (BOr BFalse b2, s) = (b2, s)
bSmallStep (BOr b1 b2, s) = let (b1', s') = bSmallStep (b1, s) in (BOr b1' b2, s')
bSmallStep (BEq e1 e2, s)
    | isFinalA e1 && isFinalA e2 = let (ANum v1, _) = interpretA (e1,s)
                                       (ANum v2, _) = interpretA (e2,s)
                                   in if v1 == v2 then (BTrue, s) else (BFalse, s)
    | isFinalA e1 = let (e2', s') = aSmallStep(e2,s) in (BEq e1 e2', s')
    | otherwise   = let (e1', s') = aSmallStep(e1,s) in (BEq e1' e2, s')
bSmallStep (b, s) = (b, s)

-- Check if a boolean expression is a final value.
isFinalB :: BExp -> Bool
isFinalB BTrue  = True
isFinalB BFalse = True
isFinalB _      = False

-- Repeatedly apply bSmallStep until a final value is reached.
interpretB :: (BExp, State) -> (BExp, State)
interpretB (b, s) = if isFinalB b then (b, s) else interpretB (bSmallStep (b, s))

----------
-- Part 5: Examples
----------

initialState :: State
initialState = [("x", 10), ("y", 5), ("z", 0)]

-- Example 1: (3 + (x + y))
aexp_example :: AExp
aexp_example = AAdd (ANum 3) (AAdd (AVar "x") (AVar "y"))
-- To run: interpretA (aexp_example, initialState)
-- Expected: (ANum 18, [("x",10),("y",5),("z",0)])

-- Example 2: (true && !false) && !(true && !true)
bexp_example :: BExp
bexp_example = BAnd (BAnd BTrue (BNot BFalse)) (BNot (BAnd BTrue (BNot BTrue)))
-- To run: interpretB (bexp_example, initialState)
-- Expected: (BTrue, [("x",10),("y",5),("z",0)])

-- Example of a factorial program
-- y = 1;
-- while (!(x == 1)) {
--   y = y * x;
--   x = x - 1;
-- }
factorial_prog :: CExp
factorial_prog = CSeq (CAssign "y" (ANum 1))
                      (CWhile (BNot (BEq (AVar "x") (ANum 1)))
                              (CSeq (CAssign "y" (AMul (AVar "y") (AVar "x")))
                                    (CAssign "x" (ASub (AVar "x") (ANum 1)))))
-- The evaluator for commands (cSmallStep) is not fully defined here,
-- but this shows how a program would be represented. 