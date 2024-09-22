--https://projecteuler.net/problem=4

--Largest Palindrome Max to Min
largestPalindromeProduct :: Int -> Int -> Int -> Int -> Int -> Int
largestPalindromeProduct l i m x y = if x * y < l || isPalindrome product
  then if y == i
    then l
    else if m * (y - 1) < l
      then l
      else largestPalindromeProduct' m (y - 1)
  else largestPalindromeProduct' (x - 1) y
  where 
    largestPalindromeProduct' = largestPalindromeProduct biggerPalindrome i m
    biggerPalindrome = if isPalindrome product
      then max l product
      else l
    product = x * y

--Largest Palindrome Initial to Max
--
--largestPalindromeProduct :: Int -> Int -> Int -> Int -> Int -> Int
--largestPalindromeProduct l i m x y = if x == m
--  then if y == m
--    then l
--    else largestPalindromeProduct' i (y + 1)
--  else largestPalindromeProduct' (x + 1) y
--  where 
--    largestPalindromeProduct' = largestPalindromeProduct biggerPalindrome i m
--    biggerPalindrome = if isPalindrome product
--      then max l product
--      else l
--    product = x * y

isPalindrome :: Int -> Bool
isPalindrome x = x == (read .reverse . show $ x)
