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