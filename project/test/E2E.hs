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
import Control.Monad (forM, forM_, void, when)
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

-- | Register, retrying until the server acknowledges.
--
-- INIT is a single UDP datagram with nothing layered on top to recover it, so
-- one drop would otherwise strand a client unregistered for good: it would sit
-- in its receive loop while nobody could look it up. Real clients need this
-- retry too; here it keeps the scenarios from failing over a lost packet
-- rather than over the behaviour under test.
registerRetrying :: Text -> Text -> IO Bool
registerRetrying user password = go (8 :: Int)
  where
    go 0 = pure False
    go n = do
      Net.disconnect -- drop the previous socket before taking a new port
      opened <- Net.connect user password
      if not opened
        then pure False
        else do
          acked <- awaitEvent 10 initResponse
          case acked of
            Just () -> pure True
            Nothing -> go (n - 1)

-- | Re-announce on the existing socket until the server acknowledges, the way
-- 'PeerChat.App' does on a timer. Unlike 'registerRetrying' this never
-- reconnects, so the port stays put.
announceUntilAcknowledged :: Int -> IO Bool
announceUntilAcknowledged 0 = pure False
announceUntilAcknowledged n = do
  announced <- Net.register
  if not announced
    then pure False
    else do
      acked <- awaitEvent 10 initResponse
      case acked of
        Just () -> pure True
        Nothing -> announceUntilAcknowledged (n - 1)

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
    stop = terminateProcess `andThen` waitForProcess

-- | Terminate then reap, discarding the exit code.
andThen :: (ProcessHandle -> IO ()) -> (ProcessHandle -> IO a) -> ProcessHandle -> IO ()
andThen kill reap ph = kill ph >> void (reap ph)

-- | Start the coordination server, waiting until it actually holds the port.
--
-- The protocol hardcodes UDP 2137, so an instance from a previous run that has
-- not finished shutting down will still own it. A server that loses the race
-- exits immediately, which is detectable -- so retry rather than carry on and
-- fail later with a misleading message about registrations going unanswered.
withServer :: FilePath -> IO a -> IO a
withServer exe action = bracket start stop (const action)
  where
    start = go (20 :: Int)

    go :: Int -> IO ProcessHandle
    go 0 =
      assertFailure
        "the coordination server could not bind UDP port 2137 - is another one running?"
    go attemptsLeft = do
      (_, _, _, ph) <- createProcess (proc exe []) {std_out = NoStream, std_err = NoStream}
      threadDelay 300000
      finished <- getProcessExitCode ph
      case finished of
        -- Still running, so bind() succeeded and it owns the port.
        Nothing -> pure ph
        Just _ -> threadDelay 300000 >> go (attemptsLeft - 1)

    stop = terminateProcess `andThen` waitForProcess

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
--
-- He answers up to ten messages rather than exiting after the three the first
-- scenario sends, so that he outlives Alice restarting in
-- 'reconnectingKeepsPeersReachable'. 'withChild' shuts him down either way.
runBobPeer :: IO ()
runBobPeer = do
  registered <- registerRetrying "bob" "pw-bob"
  when registered (replyLoop (10 :: Int) (600 :: Int))
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
  withServer serverExe $ withChild bobSpec $ do
    let dbPath = dir </> "alice.db"

    -- Session one: Alice registers, finds Bob, and talks to him.
    withStore dbPath $ \conn -> do
      registered <- registerRetrying "alice" "pw-alice"
      assertBool "the server never acknowledged alice's registration" registered

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

-- | Alice restarts, as she would after closing and reopening the client. She
-- comes back on a fresh ephemeral port, so both sides have to notice:
--
--   * the server must let a known username back in and adopt the new address,
--     rather than rejecting it as already taken;
--   * Bob must overwrite the address he already holds for her, rather than
--     keeping the dead one.
--
-- Miss either and Alice is unreachable until the server itself is restarted,
-- while every send still reports success -- UDP never says otherwise.
reconnectingKeepsPeersReachable :: IO ()
reconnectingKeepsPeersReachable = withSystemTempDirectory "peerchat-reconnect" $ \dir -> do
  serverExe <- buildServer dir
  setEnv "PEERCHAT_SERVER_IP" "127.0.0.1"
  Net.disconnect

  bobSpec <- bobProcess
  withServer serverExe $ withChild bobSpec $ do
    -- First session, to get an address for Alice registered with the server
    -- and cached by Bob.
    firstOk <- registerRetrying "alice" "pw-alice"
    assertBool "the server never acknowledged the first registration" firstOk

    foundFirst <- discoverPeer "bob" "pw-bob"
    assertBool "alice never discovered bob" foundFirst
    sentFirst <- Net.sendMessage "bob" "first session"
    assertBool "alice could not send in the first session" sentFirst
    replyFirst <- awaitEvent 40 (messageFrom "bob")
    assertEqual "no reply in the first session" (Just "re: first session") replyFirst

    -- Restart: a new socket means a new port, under the same username.
    secondOk <- registerRetrying "alice" "pw-alice"
    assertBool
      "the server turned away a returning user instead of updating her address"
      secondOk

    foundAgain <- discoverPeer "bob" "pw-bob"
    assertBool "alice never re-discovered bob after restarting" foundAgain
    sentSecond <- Net.sendMessage "bob" "second session"
    assertBool "alice could not send after restarting" sentSecond

    -- The real check: Bob's reply has to reach the new port. Before the fix he
    -- answered the address from the first session, which is now dead.
    replySecond <- awaitEvent 40 (messageFrom "bob")
    assertEqual
      "bob replied to alice's old address, so nothing came back"
      (Just "re: second session")
      replySecond

-- | A client that announces itself while the server is unreachable must still
-- end up registered once the server appears.
--
-- This is the case that made the retry necessary: INIT is one UDP datagram
-- with no retransmission, so a client started first -- or one whose packet is
-- simply dropped -- would otherwise run happily forever while being invisible
-- to everyone looking it up.
registrationSurvivesALostAnnouncement :: IO ()
registrationSurvivesALostAnnouncement = withSystemTempDirectory "peerchat-register" $ \dir -> do
  serverExe <- buildServer dir
  setEnv "PEERCHAT_SERVER_IP" "127.0.0.1"
  Net.disconnect

  -- Announce into the void: nothing is listening on 2137 yet.
  opened <- Net.connect "alice" "pw-alice"
  assertBool "opening a socket should not require a reachable server" opened
  portBefore <- Net.getPort

  lost <- awaitEvent 10 initResponse
  assertEqual "nothing should answer while the server is down" Nothing lost

  withServer serverExe $ do
    -- Exactly what the client's network thread does on a timer: keep
    -- announcing until acknowledged. A single retry is not enough to assert
    -- on, since that datagram can be dropped in turn -- which is the whole
    -- reason the retry loop exists.
    acked <- announceUntilAcknowledged 10
    assertBool "the retried announcements all went unanswered" acked

    -- And it must have happened on the original socket, so the address the
    -- server now hands out for us is one we are actually listening on.
    portAfter <- Net.getPort
    assertEqual "re-announcing must not change our port" portBefore portAfter

  Net.disconnect

-- | Letting a returning user back in must not let a stranger take the name:
-- reconnecting is allowed only with the password the username was claimed
-- with.
usernamesArePasswordProtected :: IO ()
usernamesArePasswordProtected = withSystemTempDirectory "peerchat-takeover" $ \dir -> do
  serverExe <- buildServer dir
  setEnv "PEERCHAT_SERVER_IP" "127.0.0.1"
  Net.disconnect

  withServer serverExe $ do
    claimed <- registerRetrying "alice" "pw-alice"
    assertBool "the server never acknowledged the registration" claimed

    -- Someone else claims the name from a different port with a wrong password.
    Net.disconnect
    reopened <- Net.connect "alice" "wrong-password"
    assertBool "opening a socket should still succeed" reopened

    impostorAck <- awaitEvent 15 initResponse
    assertEqual
      "the server let an impostor take an existing username"
      Nothing
      impostorAck

e2eTests :: TestTree
e2eTests =
  -- Generous ceiling: the scenario compiles C, spawns two processes and waits
  -- on real network round-trips.
  localOption (mkTimeout 120000000) $
    -- Sequential: every scenario drives the one global connection in the C
    -- library and binds the hardcoded server port.
    dependentTestGroup
      "end-to-end"
      AllFinish
      [ testCase "two peers meet, exchange three messages each way, and the history survives a restart" fullConversation,
        testCase "a restarted client stays reachable at its new address" reconnectingKeepsPeersReachable,
        testCase "an existing username cannot be claimed with the wrong password" usernamesArePasswordProtected,
        testCase "a client that starts before the server still gets registered" registrationSurvivesALostAnnouncement,
        testCase "a message to an unreachable peer is refused but still stored" offlinePeer,
        testCase "the C library exposes exactly one connection per process" singleConnectionPerProcess
      ]
