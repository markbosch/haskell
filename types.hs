-- Exerc. chp 3

-- :type ['a', 'b', 'c'] -> ['a', 'b', 'c'] :: [Char] - List of Char
-- :type ('a', 'b', 'c') -> ('a','b','c') :: (Char, Char, Char) - Tuple of Char
-- :type [(False, '0'], (True, '1')] -> [(False, '0'], (True, '1')] :: [(Bool, Char)]
-- :type ([True, False], ['0', '1']) -> ([True, False], ['0', '1']) :: ([Bool], [Char]) - Tuple with List of Bool and List of Char
-- :type [tail, init, reverse] -> [tail, init, reverse] :: [[a] -> [a]]

-- Function

add_ :: (Int, Int) -> Int
add_ (x,y) = x + y
-- add (3, 4)

zeroto :: Int -> [Int] -- function with n as input (Int) and returns a list of ints as output
zeroto n = [0..n]
-- zeroto 5

-- Curried functions
-- Takes one argument at the time instead of all arguments.
-- Curried functions are the standard definition in Haskell for functions with multiple arguments

-- returns a function as output for the second arg as input
add :: Int -> (Int -> Int) -- function takes a Int (x) as input and returns a function (Int -> Int) as output which is the input for y
add x y = x + y
-- add 3 4

mult :: Int -> (Int -> (Int -> Int))
mult x y z = x * y * z
-- mult 2 3 4
-- = 24