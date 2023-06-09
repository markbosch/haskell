-- Exercises chap 2

-- math     hask
-- f(x)      f x
-- f(x,y)    f x y
-- f(g(x))   f (g x)
-- f(x,g(y)) f x (g y)
-- f(x)g(y)  f x * g y

double x = x + x
quadruple x = double (double x)

-- quadruple 5
-- take (double 2) [1,2,3,4,5]

factorial n = product [1..n] -- calc. the product of list of n elements
-- factorial 10 --> 3628800

average ns = sum ns `div` length ns -- ns --> s stands for multiple numbers
-- by putting the `` on `div` you can put the arg. on the left and right side of the function to have it more 'naturual'
-- average ns = div (sum ns) (length ns) is the same
-- average [1,2,3,4,5] --> 3

n = a `div` length xs
    where
        a = 10
        xs = [1,2,3,4,5]

-- alt for last -> last [1,2,3,4,5] = 5
-- head (reverse [1,2,3,4,5])

my_last xs = head (reverse xs)
-- my_last [1,2,3,4,5]