-- Monads and more

-- Functors

--inc :: [Int] -> [Int]
--inc []     = []
--inc (n:ns) = n+1 : inc ns

--sqr :: [Int] -> [Int]
--sqr []     = []
--sqr (n:ns) = n^2 : sqr ns

-- ^^^--- these function are the same as map

inc = map (+1)
sqr = map (^2)

-- abstract it over any type / data structure

-- parameterized type f
-- which supports a fmap of a function from a -> b
-- for type f a
-- and returns f b
-- class Functor f where
--  fmap :: (a -> b) -> f a -> f b

-- functor for a list
-- f = []
--instance Main.Functor [] where
  -- fmap :: (a -> b) -> [a] -> [b]
--  fmap = map

-- > Main.fmap (^2) [1,2,3,4]
-- [1,4,9,16]

-- this is already defined in the standard
--instance Functor Maybe where
  -- fmap :: (a -> b) -> Maybe a -> Maybe b
--  fmap _ = Nothing
--  fmap g (Just x) = Just (g x)

-- > Prelude.fmap (+1) Nothing
-- Nothing

-- > Prelude.fmap (*2) (Just 3)
-- Just 6

-- > Prelude.fmap not (Just False)
-- Just True

-- Tree
data Tree a = Leaf a | Node (Tree a) (Tree a)
              deriving Show

instance Functor Tree where
  -- fmap :: (a -> b) -> Tree a -> Tree b
  fmap g (Leaf x)   = Leaf (g x)   -- applying (g x) will give b
  fmap g (Node l r) = Node (fmap g l) (fmap g r)

-- apply length on "abc", which is a and return a Leaf b (int) because of length
-- Leaf String -> Leaf Int by applying lenght (Int)
-- > fmap length (Leaf "abc")
-- Leaf 3

-- Node (Leaf Int) (Leaf Int) -> Node (Leaf Bool) (Leaf Bool)
-- > fmap even Node (Leaf 1) (Leaf 2)
-- Node (Leaf False) (Leaf True)

-- IO functor
-- > fmap show (return True)
-- "True"

inc' :: Functor f => f Int -> f Int
inc' = fmap (+1)

-- > inc' (Just 2)
--  Just 3
-- > inc' [1,2,3,4,5]
-- [2,3,4,5,6]
-- > inc' (Node (Leaf 1) (Leaf 2))
-- Node (Leaf 2) (Leaf 3)

-- Applicative

getChars :: Int -> IO String
getChars 0 = return []
getChars n = pure (:) <*> getChar <*> getChars (n-1)

-- Monads

data Expr = Val Int | Div Expr Expr

eval :: Expr -> Int
eval (Val n)   = n
eval (Div x y) = eval x `div` eval y

-- > eval (Div (Val 1) (Val 0))
-- divide by zero

safediv :: Int -> Int -> Maybe Int
safediv _ 0 = Nothing
safediv n m = Just (n `div` m)

eval' :: Expr -> Maybe Int
eval' (Val n)   = Just n
eval' (Div x y) = case eval' x of
                     Nothing -> Nothing
                     Just n  -> case eval' y of
                                   Nothing -> Nothing
                                   Just m  -> safediv n m

-- could be re-written in a applicative style, but those not work
-- because type mismatch Maybe Int to Int
--eval :: Expr -> Maybe Int
--eval (Val n)   = pure n
--eval (Div x y) = pure safediv <*> eval x <*> eval y

-- using the bind operator >>=

eval'' :: Expr -> Maybe Int
eval'' (Val n)   = Just n
eval'' (Div x y) = eval'' x >>= \n ->
                   eval'' y >>= \m ->
                   safediv n m

-- syntatic sugar with do
-- the above can also be written with the do notation

evalDo :: Expr -> Maybe Int
evalDo (Val n)   = Just n
evalDo (Div x y) = do n <- evalDo x
                      m <- evalDo y
                      safediv n m

pairs :: [a] -> [b] ->[(a,b)]
pairs xs ys = do x <- xs
                 y <- ys
                 return (x,y)
