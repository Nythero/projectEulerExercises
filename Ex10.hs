--https://projecteuler.net/problem=10
import Helpers

sumOfPrimeBelow2Million :: Int
sumOfPrimeBelow2Million = sum $ takeWhile (<2000000) primesQ

