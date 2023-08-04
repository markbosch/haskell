import Data.Char
import Data.List

-- Higher-Order functions
--
-- Is a function that takes a function as an argument or returns a function

twice :: (a -> a) -> a -> a
twice f x = f (f x)

-- > twice (*2) 3 --> 12
-- > twice reverse [1,2,3] --> [1,2,3]

-- foldr
-- simple pattern that applies to functions which takes a list as argument
-- f []     = v
-- f (x:xs) = x # f xs   -- # is operator
--
-- e.g
-- sum []     = 0
-- sum (x:xs) = x + sum xs
--
-- could be rewritten with foldr
-- sum (x:xs) = foldr (+) 0

-- or :: [Bool] -> Bool
-- or = foldr (||) False
--
-- and :: [Bool] -> Bool
-- and = foldr (&&) True

-- Composition operator .
-- to compose functions

-- twice f x = f (f x) could be rewritten as
-- twice f = f . f

-- Binary string transmitter

type Bit = Int

bin2int :: [Bit] -> Int
-- bin2int bits = sum [w*b | (w,b) <- zip weights bits]
--                where weights = iterate (*2) 1        -- weights will produce [1,2,4,8,...]
-- > bin2int [1,0,1,1] = 13 -> 1,0,4,8
-- w * b = 1*1, 2*0, 4*1, 8*1
--
-- but can be rewritten with foldr
-- four bit binary number [a,b,c,d]
-- (1 * a) + (2 * b) + (4 * c) + (8 * d)

-- which can be restructed:
-- a + (2 * b) + (4 * c) + (8 * d)
-- a + 2 * (b + (2 * c) + (4 * d))
-- a + 2 * (b + 2 * (c + (2 * d)))
-- a + 2 * (b + 2 * (c + 2 (d + 2 * 0)))

-- base conversion
bin2int = foldr (\x y -> x + 2*y) 0

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

-- Transmission
--
-- encode a string of chars as list of bits
-- by converting each char into a Unicode number
-- and convert that into a 8 bits bin number

encode :: String -> [Bit]
encode = concat . map (make8 . int2bin . ord)

-- decoding

-- chop the encode list into lists of 8 bits numbers
chop8 :: [Bit] -> [[Bit]]
chop8 [] = []
chop8 bits = take 8 bits : chop8 (drop 8 bits)

-- decode
decode :: [Bit] -> String
decode = map (chr . bin2int) . chop8

-- transmit
transmit :: String -> String
transmit = decode . channel . encode

channel :: [Bit] -> [Bit]
channel = id

-- transmit "hello world"
-- "hello world"
--


-- Voting algorithms
-- - First past the post system
-- - alternative vote system

-- First past the post
votes :: [String]
votes = ["Red", "Blue", "Green", "Blue", "Blue", "Red"]

-- higher-order functions by selecting all elem. from
-- list that are equal to the target value.
count :: Eq a => a -> [a] -> Int
count x = length . filter (== x)

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x : filter (/= x) (rmdups xs)

-- sort comes from Data.List
result :: Ord a => [a] -> [(Int,a)]
result vs = sort [(count v vs, v) | v <- rmdups vs]

-- select the second componend of the last result
winner :: Ord a => [a] -> a
winner = snd . last . result

-- > winner votes
-- > "Blue"

-- Alternative voting system

ballots :: [[String]]
ballots = [["Red", "Green"],
           ["Blue"],
           ["Green", "Red", "Blue"],
           ["Blue", "Green", "Red"],
           ["Green"]]

rmempty :: Eq a => [[a]] -> [[a]]
rmempty = filter (/= [])

elim :: Eq a => a -> [[a]] -> [[a]]
elim x = map (filter (/= x))

rank :: Ord a => [[a]] -> [a]
rank = map snd . result . map head

winner' :: Ord a => [[a]] -> a
winner' bs = case rank (rmempty bs) of
                [c]     -> c
                (c:cs)  -> winner' (elim c bs)

-- Exercises
--
-- 1. Show how the list comprehesion [f x | x <- xs, p x] can be re-expressed
--    using map and filter

-- map f (filter p xs)

-- 3. redefine `map f` and `filter p` using foldr
-- map f = foldr (\x xs -> f x : xs) []
-- filter p = foldr (\x xs -> if p then x:xs else xs) []