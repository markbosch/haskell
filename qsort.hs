-- Quick sort a list
qsort [] = []
qsort (x:xs) = qsort ys ++ [x] ++ qsort zs
                where
                  ys = [a | a <- xs, a <= x]
                  zs = [b | b <- xs, b > x]

-- usage
-- qsort [3, 1, 2, 4]
-- qsort "zyx"
-- qsort (reverse [1..10])
-- qsort (True, False, True, False)