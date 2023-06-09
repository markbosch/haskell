-- sum a list of numbers

_sum [] = 0
_sum (n:ns) = n + _sum ns