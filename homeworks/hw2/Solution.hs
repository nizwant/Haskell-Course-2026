import GHC.Real (underflowError)

data Sequence a = Empty | Single a | Append (Sequence a) (Sequence a)

-- Task 1
instance Functor Sequence where
  fmap :: (a -> b) -> Sequence a -> Sequence b
  fmap _ Empty = Empty
  fmap f (Single x) = Single (f x)
  fmap f (Append x y) = Append (fmap f x) (fmap f y)

-- Task 2
instance Foldable Sequence where
  foldMap :: (Monoid m) => (a -> m) -> Sequence a -> m
  foldMap _ Empty = mempty
  foldMap f (Single x) = f x
  foldMap f (Append x y) = foldMap f x <> foldMap f y

seqToList :: Sequence a -> [a]
seqToList = foldr (:) []

seqLength :: Sequence a -> Int
seqLength = foldl (\c _ -> c + 1) 0

-- Task 3
instance Semigroup (Sequence a) where
  (<>) :: Sequence a -> Sequence a -> Sequence a
  seq1 <> seq2 = Append seq1 seq2

instance Monoid (Sequence a) where
  mempty :: Sequence a
  mempty = Empty

-- Task 4
tailElem :: (Eq a) => a -> Sequence a -> Bool
tailElem x seq = go [seq]
  where
    go [] = False
    go (s : ss) =
      case s of
        Empty -> go ss
        Single y -> (x == y) || go ss
        Append l r -> go (l : r : ss)

-- Task 5
tailToList :: Sequence a -> [a]
tailToList seq = go [seq] []
  where
    go [] acc = reverse acc
    go (s : ss) acc =
      case s of
        Empty -> go ss acc
        Single x -> go ss (x : acc)
        Append l r -> go (l : r : ss) acc

-- Task 6

-- Task 7
-- (a)
myReverse :: [a] -> [a]
myReverse [] = []
myReverse xs = foldl (flip (++)) [] [xs]

-- (b)
myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile p = foldr fun []
  where
    fun x acc
      | p x = x : acc
      | otherwise = []

-- (c)
decimal :: [Int] -> Int
decimal = foldl (\acc d -> acc * 10 + d) 0

-- Task 8
-- (a)
helper :: (Eq a) => a -> [(a, Int)] -> [(a, Int)]
helper x [] = [(x, 1)]
helper x ((letter, number) : xs)
  | x == letter = (letter, number + 1) : xs
  | otherwise = (x, 1) : (letter, number) : xs

encode :: (Eq a) => [a] -> [(a, Int)]
encode = foldr helper []

-- (b)
decode :: [(a, Int)] -> [a]
decode = foldr (\(letter, number) acc -> replicate number letter ++ acc) []

main :: IO ()
main = putStrLn "Hello"