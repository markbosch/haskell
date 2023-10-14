-- 1. Define an instance of the Functor class for the following type of binary trees

data Tree a = Leaf | Node (Tree a) a (Tree a)
              deriving Show

instance Functor Tree where
  -- fmap :: (a -> b) -> Tree a -> Tree b
  fmap g Leaf = Leaf
  fmap g (Node l x r) = Node (fmap g l) (g x) (fmap g r)

-- > fmap even (Node Leaf 1 Leaf)
-- Node Leaf False Leaf
-- > fmap even (Node Leaf 2 Leaf)
-- Node Leaf True Leaf

-- 2. Complete the following instance declaration to ame the partially-applied
-- function type (a ->) into a functor:

instance Functor ((->) a) where
  -- fmap :: (b -> c) -> (a -> b) -> (a -> c)
  fmap = (.)

-- 3. Define an instance of the Applicative class for the type (a ->).

instance Applicative ((->) a) where
  -- pure :: b -> (a -> b)
  pure = const

  -- (<*>) :: (a -> b -> c) -> (a -> b) -> (a -> c)
  g <*> h = \x -> g x (h x)
