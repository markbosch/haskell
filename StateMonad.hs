-- State Monads
import Data.Char


-- Simple state
type State = Int

-- State Transformer
-- Which takes an input as its argument and produces an output
-- state as its result

--      +----+
-- --s->|    |--s'->
--      +----+
--
--type ST = State -> State

-- return also current value
--
--      +----+
--      |    |--v-->
-- --s->|    |--s'->
--      +----+

-- type ST a = State -> (a,State)

-- Turn the State Transformer (ST) into a Monad
-- this can only be done via a newtype instead of type.
-- Type can't be made into into instances of classes
-- The newtype requires a dummy constructor
newtype ST a = S (State -> (a,State))

-- Simple function to get rid of the constructor
app :: ST a -> State -> (a,State)
app (S st) x = st x

instance Functor ST where
  -- fmap :: (a -> b) -> ST a -> ST b
  fmap g st = S (\s -> let (x,s') = app st s in (g x, s'))

instance Applicative ST where
  -- pure :: a -> ST a
  pure x = S (\s -> (x,s))

  -- (<*>) :: ST -> (a -> b)
  stf <*> stx = S (\s ->
                     let (f,s') = app stf s
                         (x,s'') = app stx s' in (f x, s''))

instance Monad ST where
  -- (>>=) :: ST a -> (a -> ST b) -> ST b
  st >>= f = S (\s -> let (x,s') = app st s in app (f x) s')
  

data Tree a = Leaf a | Node (Tree a) (Tree a)
              deriving Show

tree :: Tree Char
tree = Node (Node (Leaf 'a') (Leaf 'b')) (Leaf 'c')

-- complex re-label by recursion
rlabel :: Tree a -> Int -> (Tree Int, Int)
rlabel (Leaf _)   n = (Leaf n, n+1)
rlabel (Node l r) n = (Node l' r', n'')
                      where
                        (l',n') = rlabel l n
                        (r',n'') = rlabel r n'
-- > fst (rlabel tree 0)
-- Node (Node (Leaf 0) (Leaf 1)) (Leaf 2)

-- simple, with a state transformer with a applicative style
fresh :: ST Int
fresh = S (\n -> (n, n+1))

alabel :: Tree a -> ST (Tree Int)
alabel (Leaf _) = Leaf <$> fresh
alabel (Node l r) = Node <$> alabel l <*> alabel r
                
-- > fst (app (alabel tree) 0)
-- Node (Node (Leaf 0) (Leaf 1)) (Leaf 2)

-- monadic style
mlabel :: Tree a -> ST (Tree Int)
mlabel (Leaf _)   = do n <- fresh
                       return (Leaf n)
mlabel (Node l r) = do l' <- mlabel l
                       r' <- mlabel r
                       return (Node l' r')

-- > fst (app (mlabel tree) 0)
-- Node (Node (Leaf 0) (Leaf 1)) (Leaf 2)

-- Generic functions
-- mapM :: Monad m => (a -> m b) -> [a] -> m [b]

-- convert a digit char to a number
conv :: Char -> Maybe Int
conv c | isDigit c = Just (digitToInt c)
       | otherwise = Nothing

-- > mapM conv "1234"
-- Just [1,2,3,4]
-- > mapM conv "1234a"
-- Nothing

-- filterM
-- list monad e.g. to calculate the `powerset` of a list
filterM :: Monad m => (a -> m Bool) -> [a] -> m [a]
filterM p []     = return []
filterM p (x:xs) = do b  <- p x            -- get the fist bool of the list
                      ys <- filterM p xs   -- recursive apply filterM on the tail of the list
                      return (if b then x:ys else ys)
-- > filterM (\x -> [True,False]) [1,2,3]
-- [[1,2,3],[1,2],[1,3],[1],[2,3],[2],[3],[]]

-- join -> concat monads
join :: Monad m => m (m a) -> m a
join mmx = do mx <- mmx
              x  <- mx
              return x

-- > join [[1,2],[3,4],[5,6]]
--[1,2,3,4,5,6]
-- > join (Just (Just 1))
-- Just 1
-- > join (Just Nothing)
-- Nothing
