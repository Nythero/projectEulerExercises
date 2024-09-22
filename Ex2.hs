--https://projecteuler.net/problem=2

sumOfEvenFibonaccisLessThan4M = sum evenFibonaccisLessThan4M

evenFibonaccisLessThan4M = filter even fibonaccisLessThan4M

fibonaccisLessThan4M = takeWhile (<4000000) fibonacci

fibonacci :: [Int]
fibonacci = go 0 1
    where go x y = x + y : go y (x + y)
