import Control.Monad.State
import Data.Map

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

eval :: Expr -> State (Map String Int) Int
eval = undefined

runEval :: Expr -> Int
runEval = undefined

-- Task 3

editDistM :: String -> String -> Int -> Int -> State (Map (Int, Int) Int) Int
editDistM = undefined

editDistance :: String -> String -> Int
editDistance = undefined

-- Task 4
-- Task 5
-- Task 6

main = do
  putStrLn "=== Homework 05 ==="

--   print $
