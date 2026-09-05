{-# LANGUAGE OverloadedStrings #-}

-- |
-- Process wiring: opens the database, registers with the server, starts the
-- background threads and hands control to brick.
--
-- __Why one network thread.__ The C library keeps its socket and peer table in
-- static globals, so exactly one thread may call into it. That thread is
-- 'networkThread': it is the only place @PeerChat.FFI@ is used after start-up.
-- Everything else asks it for work through a 'Command' queue and hears back
-- through brick's 'BChan'. The UI thread owns the SQLite connection on the
-- same principle -- one owner per resource, no locks needed.
module PeerChat.App
  ( Options (..),
    runApp,
  )
where

import Brick.BChan
import Brick.Main (customMain)
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM
import Control.Exception (bracket, bracket_)
import Control.Monad (forever, unless, void)
import Data.Text (Text)
import Data.Time
import GHC.IO.Handle (hDuplicate, hDuplicateTo)
import Graphics.Vty qualified as V
import Graphics.Vty.CrossPlatform (mkVty)
import PeerChat.FFI qualified as Net
import PeerChat.Store qualified as Store
import PeerChat.UI
import System.Exit (die)
import System.IO

data Options = Options
  { optUser :: Text,
    optPassword :: Text,
    optDatabase :: FilePath
  }
  deriving (Show)

-- | Drain the command queue, then poll the socket once. Nothing here blocks
-- for long: 'Net.receive' returns after 100ms at the latest, so queued
-- commands are never delayed by more than that.
--
-- 'Net.receive' is a @safe@ foreign call, so while it waits inside @select()@
-- the runtime keeps scheduling the UI thread -- which is why the executable
-- must be built with @-threaded@.
networkThread :: TQueue Command -> BChan AppEvent -> IO ()
networkThread commands out = forever $ do
  queued <- atomically (flushTQueue commands)
  mapM_ runCommand queued
  event <- Net.receive 100
  mapM_ (writeBChan out . NetEvent) event
  where
    runCommand (CmdDiscover user password) = void (Net.getUser user password)
    runCommand (CmdPing who) = void (Net.sendPing who)
    runCommand (CmdSend to body) = do
      sent <- Net.sendMessage to body
      -- The usual cause is that the server has not given us an address for
      -- this peer yet, which the user needs to know about.
      unless sent $
        writeBChan out (NetNote ("could not send to " <> to <> " - not reachable yet"))

-- | One 'Tick' a second, so the online badges age without any input. Touches
-- no shared resource, so it is safe to run on its own thread.
tickerThread :: BChan AppEvent -> IO ()
tickerThread out = forever $ do
  threadDelay 1000000
  now <- getCurrentTime
  writeBChan out (Tick now)

-- | Point stderr at a file while the action runs, restoring it afterwards.
--
-- The C library reports problems with @perror@ and @fprintf(stderr, ...)@,
-- which would otherwise be painted straight over the interface: it writes to
-- file descriptor 2, and brick has no idea. 'hDuplicateTo' redirects that
-- descriptor itself, so the C output follows it into the log instead of being
-- lost. Failures the user needs to see are surfaced in the status bar.
withStderrLog :: FilePath -> IO a -> IO a
withStderrLog path action =
  withFile path AppendMode $ \logFile -> do
    hSetBuffering logFile LineBuffering
    bracket (hDuplicate stderr) hClose $ \saved ->
      bracket_ (hDuplicateTo logFile stderr) (hDuplicateTo saved stderr) action

runApp :: Options -> IO ()
runApp opts = withStderrLog (optDatabase opts ++ ".log") $ Store.withStore (optDatabase opts) $ \conn -> do
  -- Registering happens here, before the network thread starts, so there is
  -- still only ever one thread inside the C library.
  registered <- Net.connect (optUser opts) (optPassword opts)
  unless registered (die "peerchat: could not open a socket")

  commands <- newTQueueIO
  events <- newBChan 64
  _ <- forkIO (networkThread commands events)
  _ <- forkIO (tickerThread events)

  known <- Store.listContacts conn
  now <- getCurrentTime
  zone <- getCurrentTimeZone
  history <- case known of
    [] -> pure []
    ((peer, _) : _) -> Store.loadHistory conn peer

  let initial =
        AppState
          { asUser = optUser opts,
            asStore = conn,
            asCommands = commands,
            -- Contacts are remembered across restarts, but nobody counts as
            -- online until they answer a ping in this session -- hence Nothing
            -- rather than the last_seen we just read.
            asContacts = [Contact name Nothing False | (name, _) <- known],
            asSelected = 0,
            asHistory = history,
            asInput = "",
            asNow = now,
            asLastPing = now,
            asZone = zone,
            asStatus = "connecting... use /connect <user> <password> to find a peer"
          }

  let builder = mkVty V.defaultConfig
  vty <- builder
  _ <- customMain vty builder (Just events) theApp initial
  Net.disconnect
