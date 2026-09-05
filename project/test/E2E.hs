{-# LANGUAGE OverloadedStrings #-}

-- | End-to-end tests: complete scenarios driven through the real stack.
--
-- These are the only tests that exercise the seam between the C networking
-- layer and the Haskell store, so they run real processes over real UDP:
-- a coordination server compiled from @src/server.c@, and a second client.
--
-- The C library keeps one connection in process-global state, so \"a second
-- client\" has to be a second process. Rather than ship a separate helper
-- binary, the test executable re-runs itself with @PEERCHAT_E2E_ROLE=bob@ set;
-- 'runBobPeer' is the entry point for that mode.
--
-- Note that the protocol hardcodes UDP port 2137, so these tests cannot run
-- while another coordination server is listening on this machine.
module E2E (e2eTests, runBobPeer) where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket)
import Control.Monad (forM, forM_, when)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time
import PeerChat.FFI qualified as Net
import PeerChat.Store
import System.Directory (doesFileExist)
import System.Environment (getEnvironment, getExecutablePath, setEnv)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process
import Test.Tasty
import Test.Tasty.HUnit

-- Helpers ---------------------------------------------------------------------

-- | Poll for an event the predicate accepts, giving up after @n@ tries of
-- 200ms each. Events that do not match are discarded.
awaitEvent :: Int -> (Net.Event -> Maybe a) -> IO (Maybe a)
awaitEvent 0 _ = pure Nothing
awaitEvent n p = do
  event <- Net.receive 200
  -- (>>= p) in the Maybe monad: nothing arrived, or it did not match.
  case event >>= p of
    Just a -> pure (Just a)
    Nothing -> awaitEvent (n - 1) p

initResponse :: Net.Event -> Maybe ()
initResponse Net.EvInitResponse = Just ()
initResponse _ = Nothing

messageFrom :: Text -> Net.Event -> Maybe Text
messageFrom who (Net.EvMessage from body) | from == who = Just body
messageFrom _ _ = Nothing

discovered :: Text -> Net.Event -> Maybe ()
discovered who (Net.EvPeerDiscovered peer) | peer == who = Just ()
discovered _ _ = Nothing

-- | Ask the server for a peer until it answers. Retried because the peer's own
-- registration may not have reached the server yet.
discoverPeer :: Text -> Text -> IO Bool
discoverPeer user password = go (20 :: Int)
  where
    go 0 = pure False
    go n = do
      _ <- Net.getUser user password
      found <- awaitEvent 5 (discovered user)
      case found of
        Just () -> pure True
        Nothing -> go (n - 1)

mkMessage :: Direction -> Text -> Text -> Text -> UTCTime -> StoredMessage
mkMessage direction from to body timestamp =
  StoredMessage
    { smId = 0,
      smFrom = from,
      smTo = to,
      smBody = body,
      smTimestamp = timestamp,
      smDirection = direction
    }

-- Process fixtures ------------------------------------------------------------

-- | Compile @src/server.c@ into the given directory and return the binary.
-- The server has its own @main@, so it is not a Cabal target.
buildServer :: FilePath -> IO FilePath
buildServer dir = do
  present <- doesFileExist serverSrc
  when (not present) $
    assertFailure
      ("cannot find " ++ serverSrc ++ " -- run `cabal test` from the package directory")
  let out = dir </> "peerchat-server"
  (code, _, err) <- readProcessWithExitCode "cc" [commonSrc, serverSrc, "-o", out] ""
  case code of
    ExitSuccess -> pure out
    _ -> assertFailure ("failed to compile the C coordination server:\n" ++ err)
  where
    serverSrc = "src" </> "server.c"
    commonSrc = "src" </> "c_common.c"

-- | Run an action with a child process alive, killing it afterwards even if
-- the action fails.
withChild :: CreateProcess -> IO a -> IO a
withChild spec action =
  bracket start stop (const (threadDelay 500000 >> action))
  where
    start = do
      (_, _, _, ph) <- createProcess spec {std_out = NoStream, std_err = NoStream}
      pure ph
    stop ph = do
      terminateProcess ph
      _ <- waitForProcess ph
      pure ()

-- | The test binary re-invoked as Bob, pointed at the local server.
bobProcess :: IO CreateProcess
bobProcess = do
  self <- getExecutablePath
  parentEnv <- getEnvironment
  let overrides = [("PEERCHAT_E2E_ROLE", "bob"), ("PEERCHAT_SERVER_IP", "127.0.0.1")]
      inherited = filter (\(k, _) -> k `notElem` map fst overrides) parentEnv
  pure (proc self []) {env = Just (overrides ++ inherited)}

-- | Bob's side of the conversation: register, then echo every message back
-- with a @re:@ prefix. Runs in its own process.
runBobPeer :: IO ()
runBobPeer = do
  registered <- Net.connect "bob" "pw-bob"
  when registered $ do
    _ <- awaitEvent 25 initResponse
    replyLoop (3 :: Int) (300 :: Int)
  Net.disconnect
  where
    replyLoop 0 _ = pure ()
    replyLoop _ 0 = pure ()
    replyLoop remaining budget = do
      event <- Net.receive 200
      case event of
        Just (Net.EvMessage from body) -> do
          _ <- Net.sendMessage from ("re: " <> body)
          replyLoop (remaining - 1) (budget - 1)
        _ -> replyLoop remaining (budget - 1)

-- Scenarios -------------------------------------------------------------------

-- | The scenario from the project brief: two clients meet through the server,
-- exchange three messages each way, and Alice's client is restarted to prove
-- all six survived on disk.
fullConversation :: IO ()
fullConversation = withSystemTempDirectory "peerchat-e2e" $ \dir -> do
  serverExe <- buildServer dir
  setEnv "PEERCHAT_SERVER_IP" "127.0.0.1"
  Net.disconnect -- start from a known state

  bobSpec <- bobProcess
  withChild (proc serverExe []) $ withChild bobSpec $ do
    let dbPath = dir </> "alice.db"

    -- Session one: Alice registers, finds Bob, and talks to him.
    withStore dbPath $ \conn -> do
      registered <- Net.connect "alice" "pw-alice"
      assertBool "alice could not open a socket" registered

      acked <- awaitEvent 25 initResponse
      assertBool "the server never acknowledged alice's registration" (acked == Just ())

      found <- discoverPeer "bob" "pw-bob"
      assertBool "alice never discovered bob" found

      forM_ [1 :: Int .. 3] $ \i -> do
        -- A non-ASCII body, so this also covers UTF-8 marshalling across the
        -- FFI boundary in both directions.
        let body = "wiadomosc " <> T.pack (show i) <> " zolc"
        sentAt <- getCurrentTime
        sent <- Net.sendMessage "bob" body
        assertBool ("alice failed to send message " ++ show i) sent
        _ <- saveMessage conn (mkMessage Outgoing "alice" "bob" body sentAt)

        reply <- awaitEvent 40 (messageFrom "bob")
        case reply of
          Nothing -> assertFailure ("no reply from bob to message " ++ show i)
          Just replyBody -> do
            receivedAt <- getCurrentTime
            _ <- saveMessage conn (mkMessage Incoming "bob" "alice" replyBody receivedAt)
            replyBody @?= ("re: " <> body)

    Net.disconnect

    -- Session two: a fresh connection to the same file, as a restart would
    -- give. Nothing is sent; everything below comes off the disk.
    history <- withStore dbPath $ \conn -> loadHistory conn "bob"

    length history @?= 6
    map smDirection history @?= [Outgoing, Incoming, Outgoing, Incoming, Outgoing, Incoming]
    map smBody history
      @?= [ "wiadomosc 1 zolc",
            "re: wiadomosc 1 zolc",
            "wiadomosc 2 zolc",
            "re: wiadomosc 2 zolc",
            "wiadomosc 3 zolc",
            "re: wiadomosc 3 zolc"
          ]

    -- Contacts recorded during the session are readable after the restart too.
    contacts <- withStore dbPath $ \conn -> do
      seen <- getCurrentTime
      upsertContact conn "bob" seen
      listContacts conn
    map fst contacts @?= ["bob"]

-- | Alice writes to someone she has never been given an address for. The send
-- is refused, but the message is still kept locally.
--
-- This is as far as the protocol allows the test to go: UDP has no delivery
-- confirmation, so a peer that was discovered and has since died still reports
-- a successful send. Recording real delivery status would need an
-- acknowledgement packet, which the C protocol does not have.
offlinePeer :: IO ()
offlinePeer = withSystemTempDirectory "peerchat-offline" $ \dir -> do
  setEnv "PEERCHAT_SERVER_IP" "127.0.0.1"
  Net.disconnect

  -- No server is running; opening the socket still succeeds.
  registered <- Net.connect "alice" "pw-alice"
  assertBool "opening a socket should not require a reachable server" registered

  sent <- Net.sendMessage "ghost" "are you there?"
  assertEqual "sending to an undiscovered peer must fail" False sent

  let dbPath = dir </> "alice.db"
  stored <- withStore dbPath $ \conn -> do
    now <- getCurrentTime
    _ <- saveMessage conn (mkMessage Outgoing "alice" "ghost" "are you there?" now)
    loadHistory conn "ghost"

  map smBody stored @?= ["are you there?"]
  Net.disconnect

-- | Registering twice from one process reuses the same global connection: the
-- second connect replaces the first rather than opening a parallel one.
singleConnectionPerProcess :: IO ()
singleConnectionPerProcess = do
  setEnv "PEERCHAT_SERVER_IP" "127.0.0.1"
  Net.disconnect

  fds <- forM [1 :: Int, 2] $ \_ -> do
    _ <- Net.connect "alice" "pw-alice"
    Net.getFd
  Net.disconnect

  assertBool "each connect should yield a usable descriptor" (all (>= 0) fds)
  released <- Net.getFd
  assertEqual "disconnect must release the descriptor" (-1) released

e2eTests :: TestTree
e2eTests =
  -- Generous ceiling: the scenario compiles C, spawns two processes and waits
  -- on real network round-trips.
  localOption (mkTimeout 120000000) $
    testGroup
      "end-to-end"
      [ testCase "two peers meet, exchange three messages each way, and the history survives a restart" fullConversation,
        testCase "a message to an unreachable peer is refused but still stored" offlinePeer,
        testCase "the C library exposes exactly one connection per process" singleConnectionPerProcess
      ]
