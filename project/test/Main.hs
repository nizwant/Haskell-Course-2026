-- | Test entry point.
--
-- The end-to-end tests need a second chat client, and the C library allows
-- only one connection per process, so this binary doubles as that second
-- client when @PEERCHAT_E2E_ROLE@ is set. See "E2E".
module Main (main) where

import E2E qualified
import Properties qualified
import System.Environment (lookupEnv)
import Test.Tasty
import Test.Tasty.Runners (NumThreads (..))
import Unit qualified

main :: IO ()
main = do
  role <- lookupEnv "PEERCHAT_E2E_ROLE"
  case role of
    Just "bob" -> E2E.runBobPeer
    _ -> defaultMain tests

tests :: TestTree
tests =
  testGroup
    "PeerChat"
    [ Unit.storeUnitTests,
      Properties.propertyTests,
      -- Everything below drives the single global connection inside the C
      -- library, so these must not run concurrently with each other.
      localOption (NumThreads 1) $
        testGroup
          "networking"
          [ Unit.ffiUnitTests,
            E2E.e2eTests
          ]
    ]
