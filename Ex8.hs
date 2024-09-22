--https://projecteuler.net/problem=8

import System.IO
import Control.Monad
import Data.Char (digitToInt)

main = do
        handle <- openFile "Ex8.txt" ReadMode
        contents <- hGetContents handle
        let tmp = filter (/='\n') contents
        let response = largestProductInSerie tmp 13
        print response
        hClose handle

largestProductInSerie :: String -> Int -> Int
largestProductInSerie s n = largestProductInSerie' s n 0

largestProductInSerie' :: String -> Int -> Int -> Int
largestProductInSerie' s n m
  | length s < n        = m
  | otherwise           = largestProductInSerie' (tail s) n maxProduct
  where maxProduct   = max m (productN s n)
        
productN :: String -> Int -> Int
productN s n = product (numbers s n)

numbers :: String -> Int -> [Int]
numbers s n = map digitToInt (numbersS s n) :: [Int]

numbersS :: String -> Int -> String
numbersS s n = take n s 
