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

-- Choices

-- Alternative -> empty or <|>
-- this could also be done via MonadPlus from Control.Monad
-- with mzero and mplus
instance Alternative Parser where
  -- empty :: Parser a
  empty = P (\inp -> [])
  -- (<|>) :: Parser a -> Parser a -> Parser a
  p <|> q = P (\inp -> case parse p inp of
                          []        -> parse q inp
                          [(v,out)] -> [(v,out)])

-- > parse empty "abc"
-- []
-- > parse (item <|> return 'd') "abc"
-- [('a',"bc")]
-- > parse (empty <|> return 'd') "abc"
-- [('d',"abc")]

-- Building some useful parsers with the above building
-- blocks icw primitives

sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item
           if p x then return x else empty

-- digit
digit :: Parser Char
digit = sat isDigit

-- lower
lower :: Parser Char
lower = sat isLower

-- upper
upper :: Parser Char
upper = sat isUpper

-- letter
letter :: Parser Char
letter = sat isAlpha

-- alphanum
alphanum :: Parser Char
alphanum = sat isAlpha

-- char
char :: Char -> Parser Char
char x = sat (== x)

-- > parse (char 'a') "abc"
-- [('a',"bc")]

-- string
string :: String -> Parser String
string []     = return []
string (x:xs) = do char x
                   string xs
                   return (x:xs)

-- > parse (string "abc") "abcdefg"
-- [("abc","defg")]
-- > parse (string "abc") "abcdefg"
-- [("abc","defg")]

-- `many` and `some` parsers
-- apply a parser as many times as possible untils it fails
-- where the difference between many and some is that many
-- permits zero or more applications while some requires
-- at least one successfull application

-- The `many` and `some` are already defined in the
-- class Applicative f => Alternative f where
--   many :: f a -> f [a]
--   some :: f a -> f [a]

-- > parse (many digit) "123abc"
-- [("123","abc")]
-- > parse (many digit) "abc"
-- [("","abc")]
-- > parse (some digit) "abc"
-- []

-- identifier (variable names)
ident :: Parser String
ident = do x  <- lower
           xs <- many alphanum
           return (x:xs)

-- natural number
nat :: Parser Int
nat = do xs <- some digit
         return (read xs)

-- space
space :: Parser ()
space = do many (sat isSpace)
           return ()

-- > parse ident "abc def"
-- [("abc"," def")]

-- > parse nat "123 abc"
--[(123," abc")]

-- > parse space "    abc"
-- [((),"abc")]

-- Integer
int :: Parser Int
int = do char '-'
         n <- nat
         return (-n)
       <|> nat  

-- > parse int "-3 abc"
-- [(-3," abc")]

-- Handling spacing
token :: Parser a -> Parser a
token p = do space
             v <- p
             space
             return v

identifier :: Parser String
identifier = token ident

natural :: Parser Int
natural = token nat

integer :: Parser Int
integer = token int

symbol :: String -> Parser String
symbol xs = token (string xs)

nats :: Parser [Int]
nats = do symbol "["
          n  <- natural
          ns <- many (do symbol ","
                         natural)
          symbol "]"
          return (n:ns)

-- parse nats " [1, 2, 3] "
-- [([1,2,3],"")]
-- > parse nats "[1, 2,]"
-- []

        
