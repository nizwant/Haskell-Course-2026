import Control.Monad.State
import Data.Map
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)

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

-- Task 4 - 6

data Location
  = Normal
  | Obstacle Int
  | Treasure Int
  | Trap Int
  | DecisionPoint [String]
  | Goal
  deriving (Show)

data GameState = GameState
  { playerPos :: Int,
    playerEnergy :: Int,
    playerScore :: Int,
    gameBoard :: Map Int Location
  }
  deriving (Show)

type AdventureGame a = StateT GameState IO a

getDiceRoll :: IO Int
getDiceRoll = do
  putStrLn "Enter dice roll (1-6):"
  input <- getLine

  case readMaybe input of
    Just n | n >= 1 && n <= 6 -> return n
    _ -> do
      putStrLn "Invalid dice roll. Try again."
      getDiceRoll

displayGameState :: GameState -> IO ()
displayGameState gs = do
  putStrLn "---------------------------"
  putStrLn ("Position: " ++ show (playerPos gs))
  putStrLn ("Energy : " ++ show (playerEnergy gs))
  putStrLn ("Score  : " ++ show (playerScore gs))
  putStrLn "---------------------------"

getPlayerChoice :: [String] -> IO String
getPlayerChoice options = do
  putStrLn "Choose an option:"

  mapM_
    (\(i, opt) -> putStrLn (show i ++ ". " ++ opt))
    (zip [1 ..] options)

  input <- getLine

  case readMaybe input of
    Just n
      | n >= 1 && n <= length options ->
          return (options !! (n - 1))
    _ -> do
      putStrLn "Invalid choice. Try again."
      getPlayerChoice options

movePlayer :: Int -> AdventureGame Int
movePlayer diceRoll = do
  gs <- get

  let newPos = playerPos gs + diceRoll
      newEnergy = playerEnergy gs - 1

  put
    gs
      { playerPos = newPos,
        playerEnergy = newEnergy
      }

  return diceRoll

makeDecision :: [String] -> AdventureGame String
makeDecision options = do
  liftIO (getPlayerChoice options)

handleLocation :: AdventureGame Bool
handleLocation = do
  gs <- get

  let location = Map.findWithDefault Normal (playerPos gs) (gameBoard gs)

  case location of
    Normal -> do
      liftIO $ putStrLn "Nothing special here."
      return False
    Obstacle penalty -> do
      liftIO $ putStrLn "You hit an obstacle!"

      modify
        ( \s ->
            s
              { playerEnergy = playerEnergy s - penalty
              }
        )

      return False
    Treasure points -> do
      liftIO $ putStrLn "You found treasure!"

      modify
        ( \s ->
            s
              { playerScore = playerScore s + points
              }
        )

      return False
    Trap points -> do
      liftIO $ putStrLn "You fell into a trap!"

      modify
        ( \s ->
            s
              { playerScore = max 0 (playerScore s - points)
              }
        )

      return False
    DecisionPoint options -> do
      choice <- makeDecision options
      liftIO $ putStrLn ("You chose: " ++ choice)
      return False
    Goal -> do
      liftIO $ putStrLn "You reached the main treasure!"
      return True

playTurn :: AdventureGame Bool
playTurn = do
  gs <- get

  if playerEnergy gs <= 0
    then do
      liftIO $ putStrLn "You ran out of energy!"
      return True
    else do
      dice <- liftIO getDiceRoll

      _ <- movePlayer dice

      handleLocation

playGame :: AdventureGame ()
playGame = do
  gs <- get
  liftIO (displayGameState gs)

  finished <- playTurn

  if finished
    then do
      finalState <- get
      liftIO $ do
        putStrLn "Game over!"
        displayGameState finalState
    else playGame

main = do
  putStrLn "=== Homework 05 ==="

--   print $
