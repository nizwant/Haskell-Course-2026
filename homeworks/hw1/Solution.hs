import Data.Function
import Data.List (delete)
import Distribution.Simple.Utils (xargs)

-- Task 1
goldbachPairs :: Int -> [(Int, Int)]
goldbachPairs n = [(p, q) | q <- [1 .. n], p <- [1 .. q], p + q == n, isPrime p, isPrime q]

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
    p = length (head a)
    n = length (head b)

-- Task 5

chooseOneFromList :: (Eq a) => [a] -> [(a, [a])]
chooseOneFromList xs = [(x, delete x xs) | x <- xs]

permutations :: (Eq a) => Int -> [a] -> [[a]]
permutations 0 _ = [[]]
permutations _ [] = []
permutations k xs = [x : t | (x, rest) <- chooseOneFromList xs, t <- permutations (k - 1) rest]

-- Task 6

-- (a)
merge :: (Ord a) => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x : xs) (y : ys)
  | y > x = x : merge xs (y : ys)
  | x > y = y : merge (x : xs) ys
  | otherwise = x : merge xs ys

-- (b)
hamming :: [Integer]
hamming = 1 : merge (merge (map (* 2) hamming) (map (* 3) hamming)) (map (* 5) hamming)

-- Task 7

power :: Int -> Int -> Int
power x n = go x n 1
  where
    go x 0 acc = acc
    go x n !acc = go x (n - 1) (x * acc)

-- Task 8

listMax :: [Int] -> Int
listMax (x : xs) = go xs x
  where
    go [] acc = acc
    go (x : xs) acc =
      let acc' = max x acc
       in acc' `seq` go xs acc'

listMax' :: [Int] -> Int
listMax' (x : xs) = go xs x
  where
    go [] acc = acc
    go (x : xs) !acc = go xs (max x acc)

-- Task 9

-- (a)
primes :: [Int]
primes = sieve [2 ..]

-- (b)
isPrime' :: Int -> Bool
isPrime' n = n `elem` primes

-- Task 10

-- (a)
mean :: [Double] -> Double
mean [] = 0
mean xs = go xs (0, 0)
  where
    go [] (acc, counter) = acc / counter
    go (x : xs) (acc, counter) = go xs (acc + x, counter + 1)

-- (b)

mean' :: [Double] -> Double
mean' [] = 0
mean' xs = go xs (0, 0)
  where
    go [] (acc, counter) = acc / counter
    go (x : xs) (!acc, !counter) = go xs (acc + x, counter + 1)

-- Bang in front of tuple is not enough, it only forces the structure
-- because this is the WHNF, it does not evaluates components

-- (c)

meanAndVar :: [Double] -> (Double, Double)
meanAndVar [] = (0, 0)
meanAndVar xs = go xs (0, 0, 0)
  where
    go [] (acc, counter, sum_of_squ) = (acc / counter, sum_of_squ / counter - acc * acc / counter / counter)
    go (x : xs) (!acc, !counter, !sum_of_squ) = go xs (acc + x, counter + 1, sum_of_squ + x * x)
