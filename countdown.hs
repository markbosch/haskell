-- The countdown problem
--
-- Given a sequence of numbers and a target number, attempt to
-- construct an expression whose value is the target, by combining
-- one or more numbers from the sequence using addition, substraction
-- mulitplication, division and parentheses.

-- operations
data Op = Add | Sub | Mul | Div

instance Show Op where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"

-- Validation including exploiting algebraic properties
valid :: Op -> Int -> Int -> Bool
valid Add x y = x <= y 
valid Sub x y = x > y
valid Mul x y = x /= 1 && y /= 1 && x <= y
valid Div x y = y /= 1 && x `mod` y == 0

-- Apply the Op
apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y

-- Expr with a simple pritty-printer
data Expr = Val Int | App Op Expr Expr

instance Show Expr where
  show (Val n)     = show n
  show (App o l r) = brak l ++ show o ++ brak r
                     where
                       brak (Val n) = show n
                       brak e       = "(" ++ show e ++ ")"

-- e.g 1 + (2 * 3)
-- show (App Add (Val 1) (App Mul (Val 2) (Val 3)))
-- > "1+(2*3)"

values :: Expr -> [Int]
values (Val n)     = [n]
values (App _ l r) = values l ++ values r

eval :: Expr -> [Int]
eval (Val n)     = [n | n > 0]
eval (App o l r) = [apply o x y | x <- eval l,
                                    y <- eval r,
                                    valid o x y]

-- > eval (App Add (Val 2) (Val 3))
-- > [5]

-- combinatorial functions

-- return all subsequences of a list, which are given by all
-- possible combinations excluding or including each element
-- of the list. e.g.
-- > subs [1,2,3]
-- > [[],[3],[2],[2,3],[1],[1,3][1,2],[1,2,3]]
subs :: [a] -> [[a]]
subs [] = [[]]
subs (x:xs) = yss ++ map (x:) yss
              where yss = subs xs

-- interleave returns all possible ways of inserting a new
-- element into a list. e.g.
-- > interleave 1 [2,3,4]
-- > [[1,2,3,4],[2,1,3,4],[2,3,1,4],[2,3,4,1]]
interleave :: a -> [a] -> [[a]]
interleave x [] = [[x]]
interleave x (y:ys) = (x:y:ys) : map (y:) (interleave x ys)

-- returns all permutations of a list
-- > perms [1,2,3]
-- > [[1,2,3],[2,1,3],[2,3,1],[1,3,2]...]
perms :: [a] -> [[a]]
perms [] = [[]]
perms (x:xs) = concat (map (interleave x) (perms xs))

-- return all possible choices from a list
choices :: [a] -> [[a]]
choices = concat . map perms . subs

-- solution
-- Indicates whether an Expression is a solution for a given
-- list of numbers.
-- e :: Expr -> (1 + 50) * (25 - 10)
solution :: Expr -> [Int] -> Int -> Bool
solution e ns n =
  elem (values e) (choices ns) && eval e == [n]


-- Brute force solution

-- Split a list by all possible ways into two non-empty lists
-- that append to give the original list
-- > split [1,2,3,4]
-- > [([1],[3,4,5]),([1,2],[3,4]),([123],[4])]
split :: [a] -> [([a],[a])]
split []     = []
split [_]    = []
split (x:xs) = ([x],xs) : [(x:ls,rs) | (ls,rs) <- split xs]

-- All possible expressions
exprs :: [Int] -> [Expr]
exprs []  = []
exprs [n] = [Val n]
exprs ns  = [e | (ls,rs) <- split ns,
                  l      <- exprs ls,
                  r      <- exprs rs,
                  e      <- combine l r]

combine :: Expr -> Expr -> [Expr]
combine l r = [App o l r | o <- ops]

ops :: [Op]
ops = [Add,Sub,Mul,Div]

solutions :: [Int] -> Int -> [Expr]
solutions ns n =
  [e | ns' <- choices ns, e <- exprs ns', eval e == [n]]

-- performance optimization
type Result = (Expr,Int)

results :: [Int] -> [Result]
results []  = []
results [n] = [(Val n,n) | n > 0]
results ns = [res | (ls,rs) <- split ns,
                     lx     <- results ls,
                     ry     <- results rs,
                     res    <- combine' lx ry]

combine' :: Result -> Result -> [Result]
combine' (l,x) (r,y) =
  [(App o l r, apply o x y) | o <- ops, valid o x y]

-- Solutions
-- example: > solutions' [1,3,7,10,25,50] 765
solutions' :: [Int] -> Int -> [Expr]
solutions' ns n =
  [e | ns' <- choices ns, (e,m) <- results ns', m == n]
