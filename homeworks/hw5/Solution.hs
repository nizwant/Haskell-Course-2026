import Control.Monad.State
import Data.Map
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)

data Instr = PUSH Int | POP | DUP | SWAP | ADD | MUL | NEG deriving (Show)

-- Task 1
execInstr :: Instr -> State [Int] ()
execInstr (PUSH x) = modify (x :)
execInstr POP = do
  st <- get
  case st of
    (_ : xs) -> put xs
    [] -> return ()
execInstr DUP = do
  st <- get
  case st of
    (x : xs) -> put (x : x : xs)
    [] -> return ()
execInstr SWAP = do
  st <- get
  case st of
    (x : y : xs) -> put (y : x : xs)
    _ -> return ()
execInstr ADD = do
  st <- get
  case st of
    (x : y : xs) -> put ((x + y) : xs)
    _ -> return ()
execInstr MUL = do
  st <- get
  case st of
    (x : y : xs) -> put ((x * y) : xs)
    _ -> return ()
execInstr NEG = do
  st <- get
  case st of
    (x : xs) -> put ((-x) : xs)
    [] -> return ()

execProg :: [Instr] -> State [Int] ()
execProg = mapM_ execInstr

runProg :: [Instr] -> [Int]
runProg prog = execState (execProg prog) []

-- Task 2

data Expr
  = Num Int
  | Var String
  | Add Expr Expr
  | Mul Expr Expr
  | Neg Expr
  | Assign String Expr -- bind the value of the expression to the name, return that value
  | Seq Expr Expr -- evaluate the left, then the right; return the value of the right
  deriving (Show)

eval :: Expr -> State (Map String Int) Int
eval (Num n) = return n
eval (Var name) = do
  gets (fromMaybe 0 . Map.lookup name)
eval (Add e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  return (v1 + v2)
eval (Mul e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  return (v1 * v2)
eval (Neg e) = do
  v <- eval e
  return (-v)
eval (Assign name expr) = do
  value <- eval expr
  modify (Map.insert name value)
  return value
eval (Seq e1 e2) = do
  _ <- eval e1
  eval e2

runEval :: Expr -> Int
runEval expr = evalState (eval expr) Map.empty

-- Task 3

editDistM :: String -> String -> Int -> Int -> State (Map (Int, Int) Int) Int
editDistM xs ys i j = do
  cache <- get

  case Map.lookup (i, j) cache of
    Just value -> return value
    Nothing -> do
      result <-
        if i == 0
          then
            return j
          else
            if j == 0
              then
                return i
              else
                if xs !! (i - 1) == ys !! (j - 1)
                  then
                    editDistM xs ys (i - 1) (j - 1)
                  else do
                    deletion <- editDistM xs ys (i - 1) j
                    insertion <- editDistM xs ys i (j - 1)
                    substitution <- editDistM xs ys (i - 1) (j - 1)

                    return (1 + minimum [deletion, insertion, substitution])

      modify (Map.insert (i, j) result)
      return result

editDistance :: String -> String -> Int
editDistance xs ys =
  evalState
    (editDistM xs ys (length xs) (length ys))
    Map.empty

-- Task 4

data GameState = GameState
  { position :: Int,
    energy :: Int,
    score :: Int
  }
  deriving (Show)

type AdventureGame a = StateT GameState IO a

-- Task 5
-- Task 6

main = do
  putStrLn "=== Homework 05 ==="

--   print $
