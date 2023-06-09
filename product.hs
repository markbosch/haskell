-- Calculate product of a list

_product [] = 1                    -- times 1
_product(n:ns) = n * _product ns   -- first item of list (n) * _product rest of list (ns) until empty * 1

-- _product [2, 3, 4] = 24