-- Basic concept
-- a function that calls itself
fac :: Int -> Int
fac 0 = 1              -- base case -> the recursion will stop here if n = 0
fac n = n * fac (n-1)

-- length
-- length :: [a] -> Int
-- length []     = 0
-- length (_:xs) = 1 + length xs  -- 1 + call it self with the remainder of the list (xs).

-- reverse
-- reverse :: [a] -> [a]
-- reverse []     = []                 -- base case, will stop if the remainder of the list is empty
-- reverse (x:xs) = reverse xs ++ [x]  -- append the first element of the list at the end of the list 



-- Exercises

-- 1.

-- 2. sumdown
sumdown :: Int -> Int
sumdown 0 = 0
sumdown n = n + sumdown (n-1)

-- 4. Euclid's algorithm - calculate the greatest common divisor

euclid :: Int -> Int -> Int
euclid a 0 = a
euclid a b = euclid b (a `mod` b)

-- 7. merge two sorted list into a single sorted list
-- merge [2,5,6] [1,3,4]
-- [2,5,6] [1,3,4]
--  ˆ       ^      two markers x and y. y is smaller then x
-- [1,
-- [2,5,6] [1,3,4]
--  ^         ˆ    move the marker of y. x is smaller then y
-- [1,2,
-- [2,5,6] [1,3,4]
--    ˆ       ^    move the marker of x. y is smaller then x
-- [1,2,3,
-- [2,5,6] [1,3,4]
--    ^         ^  move the marker of y. y is smaller then x and end of list y -> copy all xs
-- [1,2,3,4,5,6]

merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys                                -- base case if xs is empty return remainder ys
merge xs [] = xs                                -- base case if ys is empty return remainder xs
merge (x:xs) (y:ys) = if x <= y then
                         x : merge xs (y:ys)
                      else
                         y : merge (x:xs) ys