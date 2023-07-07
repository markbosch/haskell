import Data.Char

-- List comprehensions
-- Create lists based on a expression

-- The following list comprehension creates a
-- new list from x^2 such that x is drawn from the list [1,2,3,4,5]
-- [x^2 | x <- [1..5]]

-- | = such that
-- <- drawn from
-- x <- [1..5] is called a generator
--
-- It's possible to have multiple generators

-- this will create a list of pairs
-- [(x,y) | x <- [1,2,3], y <- [4,5]]
-- > [(1,4),(1,5),(2,4),(2,5),(3,4),(3,5)]

-- value from a earlier generator
-- [(x,y) | x <- [1..3], y <- [x..3]]
-- > [(1,1),(1,2),(1,3),(2,2),(2,3),(3,3)]

-- library function concat.
-- conctact multiple lists
concat :: [[a]] -> [a]
concat xss = [x | xs <- xss, x <- xs]
-- xss is a list of lists.
-- concat will create a list of x such that xs is drawn from xss and x is drawn from xs
-- take all list from xss into xs and put all the values into x
-- > Main.concat [[1..3], [4..6], [7..9]]
-- [1,2,3,4,5,6,7,8,9]

-- create a list of values from the frist value of pairs
-- discard the b
firsts :: [(a,b)] -> [a]
firsts ps = [x | (x,_) <- ps]
-- > firsts [(1,2), (3,4), (5,6)]
-- [1,3,5]

-- library function length
-- sum (add) 1's for the entire list xs and return the count
length :: [a] -> Int
length xs = sum [1 | _ <- xs]
-- > Main.length [1,2,3,4,5,6]
-- 6

-- Guards
-- Guards can filter the result of a earlier generator
-- > [x | x <- [1..10], even x]

factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]
-- > factors 15
-- [1,3,5,15]

prime :: Int -> Bool
prime n = factors n == [1,n]

primes :: Int -> [Int]
primes n = [x | x <- [2..n], prime x]

-- search for key in list of pairs (key/value) and return a list of values of those keys
find :: Eq a => a -> [(a,b)] -> [b]
find k t = [v | (k',v) <- t, k == k']
-- find 'b' [('a',1), ('b',2), ('c',3), ('b',4)]
-- [2,4]

-- combine two lists until one is exhausted.
-- zip ['a','b','c'] [1,2,3]

pairs :: [a] -> [(a,a)]
pairs xs = zip xs (tail xs)
-- pairs [1,2,3,4]
-- [(1,2),(2,3),(3,4)]

sorted :: Ord a => [a] -> Bool
sorted xs = and [x <= y | (x,y) <- pairs xs]
-- sorted [1,2,3,4] = True
-- sorted [1,3,2,4] = False

-- Determine the position of a element in a list
-- [0..] produces a infinite list of elements until xs is exhausted
-- x' is the x produced from xs
positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x',i) <- zip xs [0..], x == x']
-- positions False [True, False, True, False]
-- [1,3] index 1 and 3

-- String Comprehensions
lowers :: String -> Int
lowers xs = Main.length [x | x <- xs, x >= 'a' &&  x <= 'z']

-- > lowers "Haskell"
-- 6 

count :: Char -> String -> Int
count x xs = Main.length [x' | x' <- xs, x == x']

-- > count 's' "Missisippi"
-- 4

-- The Caesar chiper
--
-- Replace each letter in the string by the letter
-- three places futher down in the alphabet

-- return the corresponding int between 0 and 25 of a Char
let2int :: Char -> Int
let2int c = ord c - ord 'a'

-- opposite of let2int
int2let :: Int -> Char
int2let n = chr (ord 'a' + n)

shift :: Int -> Char -> Char
shift n c | isLower c = int2let ((let2int c + n) `mod` 26)   -- convert to char to int add the shift factor and return by the char of the remainder
          | otherwise = c

encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs ]

-- > encode 3 "haskell is fun"
-- "kdvnhoo lv ixq"

-- Frequency tables
table :: [Float]
table = [8.1, 1.5, 2.8, 4.2, 12.7 ,2.2, 2.0, 6.1, 7.0,
         0.2, 0.8, 4.0, 2.4, 6.7, 7.5, 1.9, 0.1, 6.0,
         6.3, 9.0, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1]

percent :: Int -> Int -> Float
percent n m = (fromIntegral n / fromIntegral m) * 100

freqs :: String -> [Float]
freqs xs = [percent (count x xs) n | x <- ['a'..'z']]
           where n = lowers xs
-- > freqs "aaabbbbbccccccccdddddeeeeeeeee"

-- Crakcing the cipher
-- compare the list based on the `chi-square statistic`

-- os = observed frequencies
-- es = expected frequencies
chisqr :: [Float] -> [Float] -> Float
chisqr os es = sum [((o-e)^2)/e | (o,e) <- zip os es]

rotate :: Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs

crack :: String -> String
crack xs = encode (-factor) xs                                  -- encode negative
  where
    factor = head (positions (minimum chitab) chitab)           -- Determine the factor which is used to encode
    chitab = [chisqr (rotate n table') table | n <- [0..25]]    
    table' = freqs xs

-- Exercises

-- 1. give an expression that calc. the sum 1^2 + 2^2+... 100^2
-- [sum [x^2] | x <- [1..100]]

-- 2. make a grid of size m x n
-- gird 1 2 -> [(0,0),(0,1),(0,2),(1,0)(1,1)(1,2)]

grid :: Int -> Int -> [(Int, Int)]
grid m n = [(x,y) | x <- [0..m], y <- [0..n]]

-- 3. define a function square which utilize the grid function that returns a coordinate square of size n
--    excluding the (0,0) to (n,n)
-- square 2 --> [(0,1),(0,2),(1,0),(1,2),(2,0),(2,1)]

square :: Int -> [(Int,Int)]
square n = [(x,y) | (x,y) <- grid n n, x /= y]  -- create a list of pairs (x,y) drawn from grid where x is not equal to y

-- 4. replicate n times the value of a
replicate :: Int -> a -> [a]
replicate n a = [a | _ <- [1..n]]

-- 5. Pythagorean
pyths :: Int -> [(Int,Int,Int)]
pyths n = [(x,y,z) | x <- [1..n],
                     y <- [1..n],
                     z <- [1..n],
                     x^2 + y^2 == z^2]

-- 6. Perfects
perfect :: Int -> Bool
perfect n = sum (init(factors n)) == n

perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], perfect x] 

-- 7. show how the list comp. [(x,y) | x <- [1,2], y <- [3,4]] can be re-expressed using two list comprehensions with one generator
-- contact [[(x,y) | x <- [1,2]] | y <- [3,4]]

-- 8. redefine the function positions with find
positions' :: Eq a => a -> [a] -> [Int]
positions' x xs = [i | (x',i) <- zip xs [0..], x == x']

-- 9. scalar product
sp :: [Int] -> [Int] -> Int
sp xs ys = sum [xs !! i * ys !! i | i <- [0..n-1]]
           where n = Prelude.length xs


