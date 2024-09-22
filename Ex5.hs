--https://projecteuler.net/problem=5
import qualified Data.Map as Map

response = Map.foldrWithKey go 1 (multipleFactors 20 Map.empty)
  where go f o p = f ^ o * p

multipleFactors :: Int -> Map.Map Int Int -> Map.Map Int Int
multipleFactors x xs = Map.unionsWith max (map factors [1..x])

factors :: Int -> Map.Map Int Int
factors 1 = Map.empty
factors x = Map.alter go (minFactor x) (factors (div x (minFactor x)))
    where
      go y = case y of Nothing -> Just 1
                       Just z  -> Just (z + 1)

minFactor :: Int -> Int
minFactor x = go x 2
  where go x f
          | x == f    = f
          | otherwise = if (mod x f) == 0
                          then f
                          else go x (f + 1)

