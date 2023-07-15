import Data.Char

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

bin2int = foldr (\x y -> x + 2*y) 0

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)


