import Control.Monad (guard)
import Control.Monad.Trans.Writer
import Data.List (delete, permutations)
import Data.Map hiding (map, valid)
import Data.Map qualified as Map hiding (map, valid)
import Distribution.Fields.LexerMonad (LexState (warnings))
import Distribution.Simple.Utils (xargs)

-- Task 1
type Pos = (Int, Int)

data Dir = N | S | E | W deriving (Eq, Ord, Show)

type Maze = Map Pos (Map Dir Pos)

-- (a)
move :: Maze -> Pos -> Dir -> Maybe Pos
move maze pos d = do
  dir_mapping <- Map.lookup pos maze
  Map.lookup d dir_mapping

-- (b)
followPath :: Maze -> Pos -> [Dir] -> Maybe Pos
followPath maze pos [] = Just pos
followPath maze pos (d : ds) = do
  new_pos <- move maze pos d
  followPath maze new_pos ds

-- (c)
safePath :: Maze -> Pos -> [Dir] -> Maybe [Pos]
safePath maze pos [] = Just [pos]
safePath maze pos (d : ds) = do
  new_pos <- move maze pos d
  path <- safePath maze new_pos ds
  return (pos : path)

-- Example maze
-- +---+---+---+---+

-- |           |   |
-- +   +---+   +   +
-- |   |       |   |
-- +   +   +---+   +
-- |       |       |
-- +---+---+---+   +
-- |               |
-- +---+---+---+---+
maze :: Maze
maze =
  Map.fromList
    [ ((0, 0), Map.fromList [(E, (1, 0)), (S, (0, 1))]),
      ((1, 0), Map.fromList [(W, (0, 0)), (E, (2, 0))]),
      ((2, 0), Map.fromList [(W, (1, 0)), (S, (2, 1))]),
      ((3, 0), Map.empty),
      ((0, 1), Map.fromList [(N, (0, 0)), (S, (0, 2))]),
      ((1, 1), Map.fromList [(E, (2, 1)), (S, (1, 2))]),
      ((2, 1), Map.fromList [(N, (2, 0)), (W, (1, 1))]),
      ((3, 1), Map.fromList [(S, (3, 2))]),
      ((0, 2), Map.fromList [(N, (0, 1)), (E, (1, 2))]),
      ((1, 2), Map.fromList [(W, (0, 2)), (N, (1, 1)), (E, (2, 2))]),
      ((2, 2), Map.fromList [(W, (1, 2)), (E, (3, 2))]),
      ((3, 2), Map.fromList [(W, (2, 2)), (N, (3, 1)), (S, (3, 3))]),
      ((0, 3), Map.empty),
      ((1, 3), Map.fromList [(E, (2, 3))]),
      ((2, 3), Map.fromList [(W, (1, 3)), (E, (3, 3))]),
      ((3, 3), Map.fromList [(W, (2, 3)), (N, (3, 2))])
    ]

-- Task 2
type Key = Map Char Char

decrypt :: Key -> String -> Maybe String
decrypt key word = traverse (`Map.lookup` key) word

decryptWords :: Key -> [String] -> Maybe [String]
decryptWords key word_list = traverse (decrypt key) word_list

key :: Key
key =
  Map.fromList
    [ ('a', 'x'),
      ('b', 'y'),
      ('c', 'z')
    ]

-- Task 3
type Guest = String

type Conflict = (Guest, Guest)

valid :: [Guest] -> [Conflict] -> Bool
valid xs conflicts =
  all ok (zip xs (tail xs ++ [head xs]))
  where
    ok (a, b) = (a, b) `notElem` conflicts && (b, a) `notElem` conflicts

seatings :: [Guest] -> [Conflict] -> [[Guest]]
seatings [] _ = [[]]
seatings guests conflict = do
  perm <- permutations guests
  guard (valid perm conflict)
  return perm

-- Task 4
data Result a = Failure String | Success a [String] deriving (Show)

-- (a)
instance Functor Result where
  fmap func (Failure x) = Failure x
  fmap func (Success a warnings) = Success (func a) warnings

instance Applicative Result where
  pure x = Success x []
  (Failure x) <*> _ = Failure x
  _ <*> (Failure x) = Failure x
  (Success f warnings_a) <*> (Success b warnings_b) = Success (f b) (warnings_a ++ warnings_b)

instance Monad Result where
  Failure x >>= _ = Failure x
  Success a warnings >>= f =
    case f a of
      Failure x -> Failure x
      Success b warnings_b -> Success b (warnings ++ warnings_b)

-- (b)
warn :: String -> Result ()
warn message = Success () [message]

failure :: String -> Result a
failure message = Failure message

-- (c)
validateAge :: Int -> Result Int
validateAge n
  | n > 150 = do
      warn "Age is above 150"
      return n
  | n < 0 = do
      failure "Age is negative"
  | otherwise = do
      return n

validateAges :: [Int] -> Result [Int]
validateAges age_list = mapM validateAge age_list

-- Task 5
data Expr = Lit Int | Add Expr Expr | Mul Expr Expr | Neg Expr

simplify :: Expr -> Writer [String] Expr
simplify expr =
  case expr of
    Lit n ->
      return (Lit n)
    Neg e -> do
      e' <- simplify e
      case e' of
        Neg inner -> do
          tell ["Double negation: --e -> e"]
          return inner
        _ ->
          return (Neg e')
    Add e1 e2 -> do
      e1' <- simplify e1
      e2' <- simplify e2
      case (e1', e2') of
        (Lit 0, e) -> do
          tell ["Add identity: 0 + e -> e"]
          return e
        (e, Lit 0) -> do
          tell ["Add identity: e + 0 -> e"]
          return e
        (Lit a, Lit b) -> do
          tell ["Constant folding: a + b"]
          return (Lit (a + b))
        _ ->
          return (Add e1' e2')
    Mul e1 e2 -> do
      e1' <- simplify e1
      e2' <- simplify e2
      case (e1', e2') of
        (Lit 0, _) -> do
          tell ["Zero absorption: 0 * e -> 0"]
          return (Lit 0)
        (_, Lit 0) -> do
          tell ["Zero absorption: e * 0 -> 0"]
          return (Lit 0)
        (Lit 1, e) -> do
          tell ["Mul identity: 1 * e -> e"]
          return e
        (e, Lit 1) -> do
          tell ["Mul identity: e * 1 -> e"]
          return e
        (Lit a, Lit b) -> do
          tell ["Constant folding: a * b"]
          return (Lit (a * b))
        _ ->
          return (Mul e1' e2')

-- Task 6
newtype ZipList a = ZipList {getZipList :: [a]} deriving (Show)

instance Functor ZipList where
  fmap func (ZipList xs) = ZipList (map func xs)

instance Applicative ZipList where
  pure a = ZipList (repeat a)
  ZipList fs <*> ZipList xs = ZipList (zipWith ($) fs xs)

main = do
  putStrLn "=== Homework 03 ==="

  putStrLn "move"
  print $ move maze (0, 0) S
  print $ move maze (0, 0) N

  putStrLn "follow path"
  print $ followPath maze (0, 0) [S, S, E]
  print $ followPath maze (0, 0) [S, E]
  print $ followPath maze (0, 0) [S, E, E]

  putStrLn "safe path"
  print $ safePath maze (0, 0) [S, S, E]
  print $ safePath maze (0, 0) [S, E]
  print $ safePath maze (0, 0) [S, E, E]
  print $ safePath maze (0, 0) [S, S, E, N, E, N, W, W]

  putStrLn "decrypt"
  print $ decrypt key "abc"
  print $ decrypt key "aaabc"
  print $ decrypt key "aaadbc"

  putStrLn "decrypt words"
  print $ decryptWords key ["abc", "aab", "ccc"]
  print $ decryptWords key ["abc", "aab", "cccd"]
  print $ decryptWords key ["abc", "aab", "ccc", "d"]

  putStrLn "seatings"
  print $ seatings ["a", "b", "c"] [("a", "b")]
  print $ seatings ["a", "b", "c", "d"] [("a", "b")]
  print $ seatings ["a", "b", "c", "d"] [("a", "b"), ("d", "c")]

  putStrLn "validate ages"
  print $ validateAges [1, 2, 3, 4, 150, 160, 170, 3, 151]
  print $ validateAges [1, 2, 3, 4, 150, 160, -1, 170, 3, 151]

  putStrLn "zipList"
  print $ pure id <*> ZipList [1, 2, 3]
  print $ pure (+) <*> ZipList [1, 2, 3] <*> ZipList [10, 20, 30]
