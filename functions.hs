-- Functions

-- Decide if an integer is even
even :: Integral a => a -> Bool
even n = n `mod` 2 == 0

-- Absolute value of an integer
abs :: Int -> Int
-- See Guarded equations
-- abs n = if n >= 0 then n else -n

-- Signum
signum :: Int -> Int
-- signum n = if n < 0 then -1 else
--               if n == 0 then 0 else 1

-- Guarded equations

abs n | n >= 0     = n
      | otherwise  = -n

signum n | n < 0     = -1
         | n == 0    = 0
         | otherwise = 1

-- Pattern matching
not :: Bool -> Bool
not False = True
not True = False

-- print(Main.not False)  -- True
-- print(Main.not True)   -- False

-- (&&) :: Bool -> Bool -> Bool   -- input two Bools output Bool
-- True && True   = True
-- True && False  = False
-- False && True  = False
-- False & False  = False
--
-- Can be simplified
-- True && True = True
-- _    && _    = False  -- Wildcard -> the rest = False

-- b && b = b
-- _ && _ = False

-- List Pattern
test :: [Char] -> Bool  -- Test if a list of char contains 3 elements
test ['a', _, _] = True
test _           = False

-- test ['a', 'b', 'c'] == True
-- test ['a', 'b'] == False

-- Lambda
-- \ -> lambda sign
-- namesless functions
-- lambda can be used to avoid named functions that is only referenced once in a program
-- 
-- example
-- (\x -> x + x) 2 = 4

-- Normal function
-- add :: Int -> Int -> Int
-- add x y = x + y

-- Lambda version
add :: Int -> (Int -> Int)
add = \x -> (\y -> x + y)

-- with named function
odds :: Int -> [Int]
-- odds n = map f [0..n-1]
--         where f x = x*2 + 1

odds n = map (\x -> x*2 + 1) [0..n-1]

-- 4.8 Exercises

-- Define a function that splits a even list into halve using library
halve :: [a] -> ([a], [a])
halve xs = (take n xs , drop n xs)
           where n = length xs `div` 2

-- Define a function that takes the third element of a list
third :: [a] -> a
-- third xs = head(tail(tail xs))    -- using head and tail
-- third xs = xs !! 2                -- Index operator
third (_:_:x:_) = x                  -- Pattern matching

-- Safetail
safetail :: [a] -> [a]
-- safetail xs = if null xs then xs else tail xs    -- conditional expression
-- safetail xs | null xs = xs
--            | otherwise = tail xs                 -- Pattern matching
safetail [] = []
safetail (_:xs) = xs

-- (||) :: Bool -> Bool -> Bool    -- OR
-- True || True   = True
-- True || False  = True
-- False || True  = True
-- False || False = False

-- False || False = False
-- _ || _ = True

-- b || c | b == c    = b
--        | otherwise = True

-- rewrite mult x y z = x*y*z into a lambda expression
mult :: Int -> Int -> Int -> Int
-- mult x y z = x*y*z
mult = \x -> (\y -> (\z -> x*y*z))

-- Luhn algorithm, is used to check bank card numbers
luhnDouble :: Int -> Int
luhnDouble x = if f <= 9 then f else f - 9
               where f = x * 2

-- luhn simplified
-- Every other number apply luhnDouble, sum total and check if it can be devided by 10 (mod 10 == 0)
luhn :: Int -> Int -> Int -> Int -> Bool
luhn a b c d | sum[luhnDouble a, b, luhnDouble c, d] `mod` 10 == 0 = True
             | otherwise = False