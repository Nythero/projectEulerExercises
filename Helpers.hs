module Helpers(
  isMultipleOf,
  primes,
  primesQ,
  isPrimeWithPrimesQ)
  where

isMultipleOf :: Int -> Int -> Bool
isMultipleOf x y = mod x y == 0

isMultipleOf2 :: Int -> Int -> Bool
isMultipleOf2 x y = mod y x == 0

primes :: [Int]
primes = primes' [] 2

primes' :: [Int] -> Int -> [Int]
primes' ps n = if isPrimeWithPrimes2 ps n
    then n : primes' (ps++[n]) (n + 1)
    else primes' ps (n + 1)

isPrimeWithPrimes :: [Int] -> Int -> Bool
isPrimeWithPrimes ps n = not . any (isMultipleOf n) $ ps

isPrimeWithPrimes2 :: [Int] -> Int -> Bool
isPrimeWithPrimes2 [] n     = True
isPrimeWithPrimes2 (x:xs) n = (not $ isMultipleOf n x) || (x ^ 2 > n) || (isPrimeWithPrimes2 xs n)

primes3 :: [Int]
primes3 = primes3' [2..]

primes3' :: [Int] -> [Int]
primes3' (n:ns) = n : primes3' (filter (not . isMultipleOf2 n) ns)

isPrimeWithPrimes3 :: [Int] -> Int -> Bool
isPrimeWithPrimes3 ps n = not . any (isMultipleOf n) $ ps

primesQ :: [Int]
primesQ = primesQ' emptyQ 2

primesQ' :: Queue Int -> Int -> [Int]
primesQ' ps n = if isPrimeWithPrimesQ ps n
    then n : primesQ' newQueue (n + 1)
    else primesQ' ps (n + 1)
  where newQueue = case peekRear ps of Nothing  -> enqueue n ps
                                       Just v   -> if n > v
                                                       then moveToFront (enqueue n ps)
                                                       else enqueue n ps

debugPrimesQ' :: Queue Int -> Int -> Int -> Queue Int
debugPrimesQ' ps _ 0 = ps
debugPrimesQ' ps n i = if isPrimeWithPrimesQ ps n
    then debugPrimesQ' newQueue (n + 1) (i - 1)
    else debugPrimesQ' ps (n + 1) (i - 1)
  where newQueue = case peekRear ps of Nothing  -> enqueue n ps
                                       Just v   -> if n > v
                                                       then moveToFront (enqueue n ps)
                                                       else enqueue n ps

isPrimeWithPrimesQ :: Queue Int -> Int -> Bool
isPrimeWithPrimesQ ps n =
  case queue of Nothing   -> True
                (Just vq) -> not (isMultipleOf n (fst vq)) && (fst vq ^ 2 > n || isPrimeWithPrimesQ (snd vq) n)
    where queue = dequeue ps

primesQ2 :: [Int]
primesQ2 = primesQ2' emptyQ 2

primesQ2' :: Queue Int -> Int -> [Int]
primesQ2' ps n = if isPrimeWithPrimesQ2 ps n
    then n : primesQ2' newQueue (n + 1)
    else primesQ2' ps (n + 1)
  where newQueue = case peekRear ps of Nothing  -> enqueue n ps
                                       Just v   -> if n > v
                                                       then moveToFront (enqueue n ps)
                                                       else enqueue n ps

isPrimeWithPrimesQ2 :: Queue Int -> Int -> Bool
isPrimeWithPrimesQ2 ps n =
  case queue of Nothing   -> True
                (Just vq) -> not (isMultipleOf n (fst vq)) && (fst vq ^ 2 > n || isPrimeWithPrimesQ2 (snd vq) n)
    where queue = dequeue ps


data Queue a = Queue [a] [a] deriving Show

-- Create an empty queue
emptyQ :: Queue a
emptyQ = Queue [] []

-- Check if queue is empty
isEmptyQ :: Queue a -> Bool
isEmptyQ (Queue [] []) = True
isEmptyQ _            = False

-- Add an element to the queue
enqueue :: a -> Queue a -> Queue a
enqueue x (Queue front rear) = Queue front (x:rear)

-- Remove an element from the queue
dequeue :: Queue a -> Maybe (a, Queue a)
dequeue (Queue [] [])       = Nothing
dequeue (Queue [] rear)     = Just (head rear, Queue (reverse $ tail rear) [])
dequeue (Queue (x:xs) rear) = Just (x, Queue xs rear)

-- Peek at the front element
peekRear :: Queue a -> Maybe a
peekRear (Queue _ r) = case r of (x:xs) -> Just x
                                 _      -> Nothing

moveToFront :: Queue a -> Queue a
moveToFront (Queue front rear) = Queue (front++reverse rear) []

data QueuePart = Front | Rear

lengthQ :: QueuePart -> Queue a -> Int
lengthQ Front (Queue front _) = length front
lengthQ Rear (Queue _ rear) = length rear

