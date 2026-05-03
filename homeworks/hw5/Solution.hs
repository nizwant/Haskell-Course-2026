import Control.Monad.State

data Instr = PUSH Int | POP | DUP | SWAP | ADD | MUL | NEG

-- Task 1
execInstr :: Instr -> State [Int] ()
execInstr = undefined

execProg :: [Instr] -> State [Int] ()
execProg = undefined

runProg :: [Instr] -> [Int]
runProg = undefined

-- Task 2

main = do
  putStrLn "=== Homework 05 ==="

--   print $
