-- Basic concept
-- a function that calls itself
fac :: Int -> Int
fac 0 = 1              -- base case -> the recursion will stop here if n = 0
fac n = n * fac (n-1)


-- Exercises

-- 1.

-- 2. sumdown
sumdown :: Int -> Int
sumdown 0 = 0
sumdown n = n + sumdown (n-1)