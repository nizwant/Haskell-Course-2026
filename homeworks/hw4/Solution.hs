newtype Reader r a = Reader {runReader :: r -> a}

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

-- Retrieves the entire environment.
ask :: Reader r r
ask = Reader (\r -> r)

-- Retrieves a value derived from the environment by applying a projection,
-- e.g. `asks interestRate :: Reader BankConfig Double`.
asks :: (r -> a) -> Reader r a
asks f = Reader f

-- Runs a subcomputation in a locally modified environment. The modification
-- is only visible inside the passed Reader — once it returns, the outer
-- environment is restored (conceptually; there is no mutable state, the
-- modified environment simply goes out of scope).
local :: (r -> r) -> Reader r a -> Reader r a
local modify (Reader ra) =
  Reader (\r -> ra (modify r))

data BankConfig = BankConfig
  { interestRate :: Double, -- annual interest rate (e.g. 0.05 for 5%)
    transactionFee :: Int, -- flat fee charged per transaction
    minimumBalance :: Int -- minimum required balance on an account
  }
  deriving (Show)

data Account = Account
  { accountId :: String, -- account identifier
    balance :: Int -- current balance
  }
  deriving (Show)

-- Computes the interest accrued on the account, based on the configured rate.
-- The result should be an Int — round or truncate as you see fit, but be consistent.
calculateInterest :: Account -> Reader BankConfig Int
calculateInterest acc = do
  rate <- asks interestRate
  let bal = balance acc
  return (floor (fromIntegral bal * rate))

-- Deducts the transaction fee from the account and returns the updated account.
-- The accountId should remain unchanged.
applyTransactionFee :: Account -> Reader BankConfig Account
applyTransactionFee acc = do
  fee <- asks transactionFee
  return acc {balance = balance acc - fee}

-- Checks whether the account balance meets the configured minimum.
checkMinimumBalance :: Account -> Reader BankConfig Bool
checkMinimumBalance acc = do
  minBal <- asks minimumBalance
  return (balance acc >= minBal)

-- Runs the three operations above on a single account and combines their results.
-- The returned tuple contains:
--   * the account after the transaction fee has been applied,
--   * the interest computed from the ORIGINAL account,
--   * whether the ORIGINAL account meets the minimum balance requirement.
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
