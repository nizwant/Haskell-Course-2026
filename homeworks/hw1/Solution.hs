import Data.Function
import Distribution.Simple.Utils (xargs)

-- Task 1
goldbachPairs :: Int -> [(Int, Int)]
goldbachPairs n = [(p, q) | q <- [1 .. n], p <- [1 .. q - 1], p + q == n, isPrime p, isPrime q]

-- Task 2

coprimePairs :: [Int] -> [(Int, Int)]
coprimePairs [] = []
coprimePairs (x : xs) = [(x, y) | y <- xs, x < y, gcd x y == 1] ++ coprimePairs xs

-- Task 3

-- works for negatives, zero, one, two, ...
sieve :: [Int] -> [Int]
sieve [] = []
sieve (x : xs) = x : sieve [y | y <- xs, y `mod` x /= 0]

primesTo :: Int -> [Int]
primesTo n = sieve [2 .. n]

isPrime :: Int -> Bool
isPrime n = n `elem` primesTo n

-- Task 4

matMul :: [[Int]] -> [[Int]] -> [[Int]]
matMul a b = [[sum [a !! i !! k * b !! k !! j | k <- [0 .. p - 1]] | j <- [0 .. (n - 1)]] | i <- [0 .. (m - 1)]]
  where
    m = length a
    p = length b
    n = length (head b)

-- Task 5
-- not done
combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations _ [] = []
combinations k (x : xs) = [x : t | t <- (k - 1) `combinations` xs] ++ k `combinations` xs

-- Task 6
-- Task 7

power :: Int -> Int -> Int
power x n = go x n 1
  where
    go x 0 acc = acc
    go x n !acc = go x (n - 1) x * acc

-- Task 8

listMax :: [Int] -> Int
listMax (x : xs) = go xs x
  where
    go [] acc = acc
    go (x : xs) acc =
      let acc' = max x acc
       in go xs acc'

listMax' :: [Int] -> Int
listMax' (x : xs) = go xs x
  where
    go [] acc = acc
    go (x : xs) !acc = go xs (max x acc)

-- Task 9

primes :: [Int]
primes = undefined

-- Task 10
