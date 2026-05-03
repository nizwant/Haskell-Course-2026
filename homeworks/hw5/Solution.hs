import Control.Monad.State
import Data.Map

data Instr = PUSH Int | POP | DUP | SWAP | ADD | MUL | NEG deriving (Show)

-- Task 1
execInstr :: Instr -> State [Int] ()
execInstr = undefined

execProg :: [Instr] -> State [Int] ()
execProg = undefined

runProg :: [Instr] -> [Int]
runProg = undefined

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
