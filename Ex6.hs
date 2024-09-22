--https://projecteuler.net/problem=6

response = (squareOfSum 100) - (sumOfSquares 100)

sumOfSquares :: Int -> Int
sumOfSquares x = sum (map (^2) [1..x])

squareOfSum :: Int -> Int
squareOfSum x = (sum [1..x]) ^ 2
