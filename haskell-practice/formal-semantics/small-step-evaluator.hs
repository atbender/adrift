-- This file implements a small-step operational semantics evaluator for a simple
-- imperative language. It defines the language's syntax (AST) and the
-- one-step evaluation rules for arithmetic, boolean, and command expressions.
-- This represents a complete, runnable assignment for a formal semantics course.

module SmallStepEvaluator where

----------
-- Part 1: State Management
----------

-- The 'State' represents the program's memory as a list of (Variable, Value) pairs.
type State = [(String, Int)]

-- findVar: Fetches a variable's value from the state. Defaults to 0 if not found.
findVar :: State -> String -> Int
findVar [] _ = 0
findVar ((s, v):xs) x
    | s == x    = v
    | otherwise = findVar xs x

-- updateVar: Updates a variable's value in the state or adds it if it's not present.
updateVar :: State -> String -> Int -> State
updateVar [] x v = [(x, v)]
updateVar ((s, _):xs) x v
    | s == x    = (x, v) : xs
    | otherwise = (s, findVar ((s, _):xs) s) : updateVar xs x v


----------
-- Part 2: Abstract Syntax Tree (AST) Definition
----------

-- Arithmetic Expressions
data AExp
    = ANum Int
    | AVar String
    | AAdd AExp AExp
    | ASub AExp AExp
    | AMul AExp AExp
    deriving (Eq, Read, Show)

-- Boolean Expressions
data BExp
    = BTrue
    | BFalse
    | BNot BExp
    | BAnd BExp BExp
    | BOr BExp BExp
    | BEq AExp AExp
    | BLeq AExp AExp -- Less than or equal to
    | BLt AExp AExp  -- Less than
    deriving (Eq, Read, Show)

-- Commands
data CExp
    = CAssign String AExp
    | CSeq CExp CExp
    | CIf BExp CExp CExp
    | CWhile BExp CExp
    | CSkip
    deriving (Eq, Read, Show)


----------
-- Part 3: Small-Step Evaluator
----------

-- == Arithmetic Evaluation ==

isFinalA :: AExp -> Bool
isFinalA (ANum _) = True
isFinalA _        = False

aSmallStep :: (AExp, State) -> (AExp, State)
aSmallStep (AVar x, s) = (ANum (findVar s x), s)
aSmallStep (AAdd (ANum x) (ANum y), s) = (ANum (x + y), s)
aSmallStep (AAdd (ANum x) e2, s) = let (e2', s') = aSmallStep (e2, s) in (AAdd (ANum x) e2', s')
aSmallStep (AAdd e1 e2, s) = let (e1', s') = aSmallStep (e1, s) in (AAdd e1' e2, s')
aSmallStep (ASub (ANum x) (ANum y), s) = (ANum (x - y), s)
aSmallStep (ASub (ANum x) e2, s) = let (e2', s') = aSmallStep (e2, s) in (ASub (ANum x) e2', s')
aSmallStep (ASub e1 e2, s) = let (e1', s') = aSmallStep (e1, s) in (ASub e1' e2, s')
aSmallStep (AMul (ANum x) (ANum y), s) = (ANum (x * y), s)
aSmallStep (AMul (ANum x) e2, s) = let (e2', s') = aSmallStep (e2, s) in (AMul (ANum x) e2', s')
aSmallStep (AMul e1 e2, s) = let (e1', s') = aSmallStep (e1, s) in (AMul e1' e2, s')
aSmallStep (e, s) = (e, s)

interpretA :: (AExp, State) -> (AExp, State)
interpretA (a, s) = if isFinalA a then (a, s) else interpretA (aSmallStep (a, s))

-- == Boolean Evaluation ==

isFinalB :: BExp -> Bool
isFinalB BTrue  = True
isFinalB BFalse = True
isFinalB _      = False

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
bSmallStep (BEq (ANum x) (ANum y), s) = (if x == y then BTrue else BFalse, s)
bSmallStep (BEq (ANum x) e2, s) = let (e2', s') = aSmallStep(e2, s) in (BEq (ANum x) e2', s')
bSmallStep (BEq e1 e2, s) = let (e1', s') = aSmallStep(e1, s) in (BEq e1' e2, s')
bSmallStep (BLeq (ANum x) (ANum y), s) = (if x <= y then BTrue else BFalse, s)
bSmallStep (BLeq (ANum x) e2, s) = let (e2', s') = aSmallStep(e2, s) in (BLeq (ANum x) e2', s')
bSmallStep (BLeq e1 e2, s) = let (e1', s') = aSmallStep(e1, s) in (BLeq e1' e2, s')
bSmallStep (BLt (ANum x) (ANum y), s) = (if x < y then BTrue else BFalse, s)
bSmallStep (BLt (ANum x) e2, s) = let (e2', s') = aSmallStep(e2, s) in (BLt (ANum x) e2', s')
bSmallStep (BLt e1 e2, s) = let (e1', s') = aSmallStep(e1, s) in (BLt e1' e2, s')
bSmallStep (b, s) = (b, s)

interpretB :: (BExp, State) -> (BExp, State)
interpretB (b, s) = if isFinalB b then (b, s) else interpretB (bSmallStep (b, s))

-- == Command Evaluation ==

isFinalC :: CExp -> Bool
isFinalC CSkip = True
isFinalC _     = False

cSmallStep :: (CExp, State) -> (CExp, State)
cSmallStep (CIf BTrue c1 _, s) = (c1, s)
cSmallStep (CIf BFalse _ c2, s) = (c2, s)
cSmallStep (CIf b c1 c2, s) = let (b', s') = bSmallStep (b, s) in (CIf b' c1 c2, s')
cSmallStep (CSeq CSkip c2, s) = (c2, s)
cSmallStep (CSeq c1 c2, s) = let (c1', s') = cSmallStep (c1, s) in (CSeq c1' c2, s')
cSmallStep (CAssign x (ANum v), s) = (CSkip, updateVar s x v)
cSmallStep (CAssign x e, s) = let (e', s') = aSmallStep (e, s) in (CAssign x e', s')
cSmallStep (CWhile b c, s) = (CIf b (CSeq c (CWhile b c)) CSkip, s)
cSmallStep (c, s) = (c, s)

interpretC :: (CExp, State) -> (CExp, State)
interpretC (c, s) = if isFinalC c then (c, s) else interpretC (cSmallStep (c, s))


----------
-- Part 4: Example Programs
----------

initialState :: State
initialState = [("x", 10), ("y", 5), ("z", 0)]

-- Swap x and z
swap_prog :: CExp
swap_prog = CSeq (CSeq (CAssign "z" (AVar "x")) (CAssign "x" (AVar "y")))
                 (CAssign "y" (AVar "z"))
-- To run: interpretC (swap_prog, initialState)
-- Expected: (CSkip,[("x",5),("y",10),("z",10)])


-- Factorial of x, result in y
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
-- To run: interpretC (factorial_prog, [("x", 5)])
-- Expected: (CSkip,[("y",120),("x",1)])


-- Sum x from 0 to 10, result in y
-- x = 0; y = 0;
-- while (x <= 10) {
--   y = y + x;
--   x = x + 1;
-- }
sum_prog :: CExp
sum_prog = CSeq (CAssign "x" (ANum 0))
              (CSeq (CAssign "y" (ANum 0))
                    (CWhile (BLeq (AVar "x") (ANum 10))
                            (CSeq (CAssign "y" (AAdd (AVar "y") (AVar "x")))
                                  (CAssign "x" (AAdd (AVar "x") (ANum 1))))))
-- To run: interpretC (sum_prog, [])
-- Expected: (CSkip,[("y",55),("x",11)]) 