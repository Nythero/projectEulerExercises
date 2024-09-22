--https://projecteuler.net/problem=3
import Helpers

primeFactors _ 1 = []
primeFactors (p:ps) x = if isMultipleOf x p
    then p : primeFactors ps (div x p)
    else primeFactors ps x

largestPrimeFactor :: [Int] -> Int -> Int
largestPrimeFactor (p:ps)      1 = p
largestPrimeFactor (p:ps) x = if isMultipleOf x p
    then largestPrimeFactor (p:ps) (div x p)
    else largestPrimeFactor ps x

primes :: [Int]
primes = primes' [] 2

primes' :: [Int] -> Int -> [Int]
primes' ps n = if isPrimeWithPrimes ps n
    then n : primes' (ps++[n]) (n + 1)
    else primes' ps (n + 1)

isPrimeWithPrimes :: [Int] -> Int -> Bool
isPrimeWithPrimes primes n = not . any (isMultipleOf n) $ primes

