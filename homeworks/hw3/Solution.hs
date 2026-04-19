import Data.Map
import Data.Map qualified as Map

type Pos = (Int, Int)

data Dir = N | S | E | W deriving (Eq, Ord, Show)

type Maze = Map Pos (Map Dir Pos)

move :: Maze -> Pos -> Dir -> Maybe Pos
move maze pos d = do
  dir_mapping <- Map.lookup pos maze
  Map.lookup d dir_mapping

followPath :: Maze -> Pos -> [Dir] -> Maybe Pos
followPath maze pos [] = Just pos
followPath maze pos (d : ds) = do
  new_pos <- move maze pos d
  followPath maze new_pos ds

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