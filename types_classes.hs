-- Types and classes

-- Types
type Pos = (Int,Int)

type Pair a = (a,a) -- parameterised
type Assoc k v = [(k,v)]

find :: Eq k => k -> Assoc k v -> v
find k t = head [v | (k',v) <- t, k == k']

-- Data
data Move = North | South | East | West -- deriving Show

move :: Move -> Pos -> Pos
move North (x,y) = (x,y+1)
move South (x,y) = (x,y-1)
move East  (x,y) = (x+1,y)
move West  (x,y) = (x-1,y)

-- > move North (1,2)
-- > (1,3)

moves :: [Move] -> Pos -> Pos
moves []     p = p
moves (m:ms) p = moves ms (move m p)

-- > moves [North,North,East,East] (1,2)
-- > (3,4)

rev :: Move -> Move
rev North = South
rev South = North
rev East = West
rev West = East

-- Data contructors with arguments

-- The constructors Circle and Rect are constuctor functions
data Shape = Circle Float | Rect Float Float deriving Show
square :: Float -> Shape
square n = Rect n n

-- > square 4
-- > Rect 4.0 4.0

area :: Shape -> Float
area (Circle r) = pi * r^2
area (Rect x y) = x * y

-- > area (Circle 3.0)
-- > 28.274334
-- > area (Rect 4.0 5.0)
-- > 20

safediv :: Int -> Int -> Maybe Int
safediv _ 0 = Nothing
safediv m n = Just (m `div` n)

safehead :: [a] -> Maybe a
safehead [] = Nothing
safehead xs = Just (head xs)

-- > safehead [1,2,3] -> Just 1
-- > safehead ["a","b","c"] Just "a"

-- Recursive types
data Nat = Zero | Succ Nat deriving Show

-- > Zero
-- > Succ Zero
-- > Succ (Succ Zero)
-- > Succ (Succ (Succ Zero))   -> 1 + (1 + (1 + 0))

nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ n) = 1 + nat2int n

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n-1))

add :: Nat -> Nat -> Nat
add Zero     n = n
add (Succ m) n = Succ (add m n)

-- > add (Succ (Succ Zero) (Succ Zero) -> 2 + 1

data List a = Nil | Cons a (List a)

len :: List a -> Int
len Nil         = 0
len (Cons _ xs) = 1 + len xs

-- Binary Tree
data Tree a = Leaf a | Node (Tree a) a (Tree a) deriving Show
t :: Tree Int
t = Node (Node (Leaf 1) 3 (Leaf 4)) 5
         (Node (Leaf 6) 7 (Leaf 9))

--       5
--      / \
--     3   7
--    / \  / \
--   1   4 6  9

-- Functions on the Tree
-- Occurs
-- will search until a leaf or if a Node is found on left or right
-- l = left, y = value, r = right
occurs :: Eq a => a -> Tree a -> Bool
occurs x (Leaf y)    = x == y 
occurs x (Node l y r)  = x == y || occurs x l || occurs x r

-- > occurs 1 t --> True
-- > occurs 8 t --> False

-- Flatten
flatten :: Tree a -> [a]
flatten (Leaf x)   = [x]
flatten (Node l x r) = flatten l ++ [x] ++ flatten r

-- Occurs optimized
occurs' :: Ord a => a -> Tree a -> Bool
occurs' x (Leaf y)                 = x == y
occurs' x (Node l y r) | x == y    = True
                       | x < y     = occurs' x l
                       | otherwise = occurs' y r

-- Derived instances
-- make data types into instance of a built-in classes like Eq or Show

data Foo = Bar | Spam
           deriving (Eq, Show, Read)

-- > Bar == Bar => True
-- > Spam == Bar => False
-- > Show Spam => "Spam"
-- > Read "Bar" :: Foo

-- Tautology checker
-- function that decides if simple logical propositions are always true

data Prop = Const Bool
          | Var Char
          | Not Prop
          | And Prop Prop
          | Imply Prop Prop   -- =>

p1 :: Prop
p1 = And (Var 'A') (Not (Var 'A'))

p2 :: Prop
p2 = Imply (And (Var 'A') (Var 'B')) (Var 'A')

p3 :: Prop
p3 = Imply (Var 'A') (And (Var 'A') (Var 'B'))

p4 :: Prop
p4 = Imply (And (Var 'A') (Imply
        (Var 'A') (Var 'B'))) (Var 'B')

-- Substitution
-- with a lookup table like [('A', False),('B', True)]

type Subst = Assoc Char Bool

-- evaluate
eval :: Subst -> Prop -> Bool
eval _ (Const b)   = b
eval s (Var x)     = find x s
eval s (Not p)     = not (eval s p)
eval s (And p q)   = eval s p && eval s q
eval s (Imply p q) = eval s p <= eval s q

-- Get all vars from a proposition
vars :: Prop -> [Char]
vars (Const _)   = []
vars (Var x)     = [x]
vars (Not p)     = vars p
vars (And p q)   = vars p ++ vars q
vars (Imply p q) = vars p ++ vars q

bools :: Int -> [[Bool]]
bools 0 = [[]]
bools n = map (False:) bss ++ map (True:) bss
         where bss = bools (n-1)

-- copy paste rmdups from highordfunc
rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x : filter (/= x) (rmdups xs)

substs :: Prop -> [Subst]
substs p = map (zip vs) (bools (length vs))
           where vs = rmdups (vars p)

isTaut :: Prop -> Bool
isTaut p = and [eval s p | s <- substs p]

-- > isTaut p1 => False
-- > isTaut p2 => True
-- > isTaut p3 => False
-- > isTaut p4 => True

-- Exercises

-- 1.

mult m Zero = Zero
mult m (Succ n) = add m (mult m n)

-- > mult (Succ (Succ Zero)) (Succ (Succ (Succ Zero)))  -> 2 x 3

-- 2.
occurs'' x (Leaf y)     = x == y
occurs'' x (Node l y r) = case compare x y of
                             LT -> occurs'' x l
                             EQ -> True
                             GT -> occurs'' x r

-- 3.
data Tree' a = Leaf' a | Node' (Tree' a) (Tree' a)

leaves :: Tree' a -> Int
leaves (Leaf' _) = 1
leaves (Node' l r) = leaves l + leaves r

balanced :: Tree' a -> Bool
balanced (Leaf' _)   = True
balanced (Node' l r) = abs(leaves l - leaves r) <= 1
                       && balanced l && balanced r

t' :: Tree' Int
t' = Node' (Leaf' 1) (Leaf' 2)

-- > balanced t'
-- > True

t'' :: Tree' Int
t'' = Node' (Node' (Leaf' 1) (Leaf' 2)) (Leaf' 3)

--          Node'
--         /     \
--     Node'      Leaf' 3
--    /    \
-- Leaf' 1  Leaf' 2
--
-- > balanced t''
-- > True

unbalancedTree :: Tree' Int
unbalancedTree = Node' (Node' (Leaf' 1) (Node' (Leaf' 2) (Leaf' 3))) (Leaf' 4)

-- > balanced unbalancedTree
-- > False

-- 4.
halve xs = splitAt (length xs `div` 2) xs

balance :: [a] -> Tree' a
balance [x] = Leaf' x
balance xs  = Node' (balance ys) (balance zs)
              where (ys,zs) = halve xs
