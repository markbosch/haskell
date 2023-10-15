-- Monadic parsing

import Control.Applicative
import Data.Char

-- Parser
newtype Parser a = P (String -> [(a,String)])

-- Remove the dummy constructor
parse :: Parser a -> String -> [(a,String)]
parse (P p) inp = p inp

-- First primitive parser `item`, which fails if the input
-- string is empty, and succeeds with the first char.
item :: Parser Char
item = P (\inp -> case inp of
                     []     -> []
                     (x:xs) -> [(x,xs)])

-- > parse item ""
-- []
-- > parse item "foo"
-- [('f', "oo")]

-- Sequencing parsers

-- Functor

-- fmap applies a function to the result value of a parser
-- if the parser succeeds and propagates the failure otherwise.
instance Functor Parser where
  -- fmap :: (a -> b) -> Parser a -> Parser b
  fmap g p = P (\inp -> case parse p inp of
                           []        -> []
                           [(v,out)] -> [(g v, out)])

-- > parse (fmap toUpper item) "abc"
-- [('A',"bc")]
-- > parse (fmap toUpper item) ""
-- []

-- Applicative

-- pure transforms a value into a parser that always succeeds
-- with this value as its result and without consuming the input
-- <*> applies a parser that retuns a function to a parser
-- that returns an argument to give a parser that returns
-- the result of applying the function to the argument. 
instance Applicative Parser where
  -- pure :: a -> Parser a
  pure v = P (\inp -> [(v,inp)])
  -- <*> :: Parser (a -> b) -> Parser a -> Parser b
  pg <*> px = P (\inp -> case parse pg inp of
                    []        -> []
                    [(g,out)] -> parse (fmap g px) out)

-- for example a parser that consumes three chars
-- and discards the second.

three' :: Parser (Char,Char)
three' = pure g <*> item <*> item <*> item
        where g x y z = (x,z)

-- > parse three "abcdefg"
-- [(('a','c'),"defg")]
-- > parse three "ab"
-- []

-- Monad

instance Monad Parser where
  -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  p >>= f = P (\inp -> case parse p inp of
                          []        -> []
                          [(v,out)] -> parse (f v) out)

-- The monadic parses enables a rewrite of three
-- with the `do` notation.
three :: Parser (Char,Char)
three = do x <- item
           item
           z <- item
           return (x,z)

-- > parse three "spam"
-- [(('s','a'),"m")]
