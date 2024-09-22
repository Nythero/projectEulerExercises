import System.IO
import Control.Monad
import Data.Char (digitToInt)

main = do
        handle <- openFile "Ex11.txt" ReadMode
        contents <- hGetContents handle
        let tmp = filter (/='\n') contents
        let response = largestProductInGrid tmp 4
        print response
        hClose handle

largestProductInGrid :: String -> Int -> Int
largestProductInGrid s n = largestProductInGrid' (gridOfNumbers s) n 0

gridOfNumbers :: String -> [[Int]]
gridOfNumbers = map (map (read::String -> Int) . words) . lines

largestProductInGrid' :: [[Int]] -> Int -> Int -> Int
largestProductInGrid' nss = 
--
--largestProductInSerie' :: String -> Int -> Int -> Int
--largestProductInSerie' s n m
--  | length s < n        = m
--  | otherwise           = largestProductInSerie' (tail s) n maxProduct
--  where maxProduct   = max m (productN s n)
--        
--productN :: String -> Int -> Int
--productN s n = product (numbers s n)
--
--numbers :: String -> Int -> [Int]
--numbers s n = map digitToInt (numbersS s n) :: [Int]
--
--numbersS :: String -> Int -> String
--numbersS s n = take n s 
--
--
