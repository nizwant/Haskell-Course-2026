newtype Reader r a = Reader {runReader :: r -> a}

-- Task 1
instance Functor (Reader r) where
  -- fmap :: (a -> b) -> Reader r a -> Reader r b
  fmap f (Reader r) = Reader (fmap f r)

instance Applicative (Reader r) where
  -- pure   :: a -> Reader r a
  pure x = Reader (const x)

  -- liftA2 :: (a -> b -> c) -> Reader r a -> Reader r b -> Reader r c
  liftA2 f (Reader ra) (Reader rb) = Reader (\r -> f (ra r) (rb r))

instance Monad (Reader r) where
  -- (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
  (Reader ra) >>= f =
    Reader
      ( \r ->
          let a = ra r
              Reader rb = f a
           in rb r
      )

-- Task 2
ask :: Reader r r
ask = Reader id

asks :: (r -> a) -> Reader r a
asks f = Reader f

local :: (r -> r) -> Reader r a -> Reader r a
local modify (Reader ra) =
  Reader (\r -> ra (modify r))

-- Task 3
data BankConfig = BankConfig
  { interestRate :: Double,
    transactionFee :: Int,
    minimumBalance :: Int
  }
  deriving (Show)

data Account = Account
  { accountId :: String,
    balance :: Int
  }
  deriving (Show)

calculateInterest :: Account -> Reader BankConfig Int
calculateInterest acc = do
  rate <- asks interestRate
  let bal = balance acc
  return (floor (fromIntegral bal * rate))

applyTransactionFee :: Account -> Reader BankConfig Account
applyTransactionFee acc = do
  fee <- asks transactionFee
  return acc {balance = balance acc - fee}

checkMinimumBalance :: Account -> Reader BankConfig Bool
checkMinimumBalance acc = do
  minBal <- asks minimumBalance
  return (balance acc >= minBal)

processAccount :: Account -> Reader BankConfig (Account, Int, Bool)
processAccount acc = do
  acc' <- applyTransactionFee acc
  interest <- calculateInterest acc
  ok <- checkMinimumBalance acc
  return (acc', interest, ok)

cfg = BankConfig {interestRate = 0.05, transactionFee = 2, minimumBalance = 100}

acc = Account {accountId = "A-001", balance = 1000}

main = do
  putStrLn "=== Homework 04 ==="
  print $ runReader (processAccount acc) cfg
