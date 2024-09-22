--https://projecteuler.net/problem=8

c1 a b = sqrt ((a ^ 2) + (b ^ 2))
c2 a b = 1000 - a - b
-- a < b < c

--  1000 - a - b = sqrt ((a ^ 2) + (b ^ 2))
--  1000 = sqrt ((a ^ 2) + (b ^ 2)) + a + b

findResult :: Int
findResult = go 1 499
  where go a b = if sqrt ((a ^ 2) + (b ^ 2)) + a + b == 1000.0
                 then round $ a * b * c2 a b
                 else if a == b
                   then go 1 (b - 1)
                   else go (a + 1) b



