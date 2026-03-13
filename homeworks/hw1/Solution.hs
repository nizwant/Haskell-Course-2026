import Data.Function

-- Task 1
goldbachPairs :: Int -> [(Int, Int)]
goldbachPairs n = [(p, q) | q <- [1 .. n], p <- [1 .. q - 1], p + q == n, isPrime p, isPrime q]

-- Task 2

coprimePairs :: [Int] -> [(Int, Int)]
coprimePairs [] = []
coprimePairs (x : xs) = [(x, y) | y <- xs, x < y, gcd x y == 1] ++ coprimePairs xs

-- Task 3

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
permutations :: Int -> [a] -> [[a]]
permutations 0 _ = [[]]
permutations _ [] = []
permutations k (x : xs) = [x : t | t <- (k - 1) `permutations` xs] ++ k `permutations` xs

-- Task 6
-- Task 7
-- Task 8
-- Task 9
-- Task 10
