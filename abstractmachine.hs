-- Abstract Machine

-- a type of simple arithmetic expression built up from integers

data Expr = Val Int | Add Expr Expr

-- value :: Expr -> Int
-- value (Val n)   = n
-- value (Val x y) = value x + value y

-- e.g (2 + 3) + 4 -> value (Add (Add (Val 2) (Val 3)) (Val 4))

-- Control stacks which contains a list of operations
type Cont = [Op]

-- Operations
data Op = EVAL Expr | ADD Int

-- evaluation function
eval :: Expr -> Cont -> Int
eval (Val n)   c = exec c n
eval (Add x y) c = eval x (EVAL y : c)

-- Execute the control stack in the context of an Integer
exec :: Cont -> Int -> Int
exec []           n = n
exec (EVAL y : c) n = eval y (ADD n : c)
exec (ADD n : c)  m = exec c (n+m)

value :: Expr -> Int
value e = eval e []

-- > value (Add (Add (Val 2) (Val 3)) (Val 4))
-- 9