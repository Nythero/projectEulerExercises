--https://projecteuler.net/problem=1
sumOfMultiplesOf3And5Below1000 :: Int
sumOfMultiplesOf3And5Below1000 = sum multiplesOf3And5Below1000

multiplesOf3And5Below1000 :: [Int]
multiplesOf3And5Below1000 = takeWhile (<1000) multiplesOf3And5

multiplesOf3And5 :: [Int]
multiplesOf3And5 = multiples [3, 5]

multiples :: [Int] -> [Int]
multiples xs = [i | i<-[1..], any (\x -> mod i x == 0) xs]
