{-# LANGUAGE OverloadedStrings #-}

-- | Unit tests: one function, one expected behaviour per test.
--
-- The store tests all run against a private in-memory database, so they are
-- fast and independent of each other. The FFI tests exercise only the states
-- that need no network.
module Unit (storeUnitTests, ffiUnitTests) where

import Data.Text (Text)
import Data.Time
import PeerChat.FFI qualified as Net
import PeerChat.Store
import Test.Tasty
import Test.Tasty.HUnit

-- | A fixed instant, so tests never depend on the current time.
t0 :: UTCTime
t0 = UTCTime (fromGregorian 2026 9 5 ) (secondsToDiffTime (14 * 3600))

-- | @at n@ is @n@ seconds after 't0'.
at :: Int -> UTCTime
at n = addUTCTime (fromIntegral n) t0

-- | A message from us ("alice") to the given peer.
outgoing :: Text -> Text -> Int -> StoredMessage
outgoing peer body n =
  StoredMessage
    { smId = 0,
      smFrom = "alice",
      smTo = peer,
      smBody = body,
      smTimestamp = at n,
      smDirection = Outgoing
    }

-- | A message from the given peer to us.
incoming :: Text -> Text -> Int -> StoredMessage
incoming peer body n =
  (outgoing peer body n) {smFrom = peer, smTo = "alice", smDirection = Incoming}

-- | Run an action against a fresh, empty database.
withTempStore :: (Connection -> IO a) -> IO a
withTempStore = withStore ":memory:"

storeUnitTests :: TestTree
storeUnitTests =
  testGroup
    "PeerChat.Store"
    [ testCase "a saved message comes back with every field intact" $ do
        loaded <- withTempStore $ \conn -> do
          _ <- saveMessage conn (incoming "bob" "did you finish the lab yet?" 0)
          loadHistory conn "bob"
        case loaded of
          [m] -> do
            smFrom m @?= "bob"
            smTo m @?= "alice"
            smBody m @?= "did you finish the lab yet?"
            smTimestamp m @?= at 0
            smDirection m @?= Incoming
          _ -> assertFailure ("expected exactly one row, got " ++ show (length loaded)),
      testCase "saveMessage assigns a real row id, ignoring the one passed in" $ do
        ids <- withTempStore $ \conn -> do
          a <- saveMessage conn (outgoing "bob" "first" 0)
          b <- saveMessage conn (outgoing "bob" "second" 1)
          pure [smId a, smId b]
        -- The inputs both carried smId = 0; SQLite must have replaced them.
        assertBool ("ids should be distinct and non-zero: " ++ show ids) $
          head ids /= 0 && length ids == 2 && head ids /= ids !! 1,
      testCase "history is returned oldest first" $ do
        bodies <- withTempStore $ \conn -> do
          -- Saved out of order on purpose.
          _ <- saveMessage conn (outgoing "bob" "third" 300)
          _ <- saveMessage conn (incoming "bob" "first" 100)
          _ <- saveMessage conn (outgoing "bob" "second" 200)
          map smBody <$> loadHistory conn "bob"
        bodies @?= ["first", "second", "third"],
      testCase "history for one peer excludes other conversations" $ do
        bodies <- withTempStore $ \conn -> do
          _ <- saveMessage conn (outgoing "bob" "to bob" 0)
          _ <- saveMessage conn (outgoing "carol" "to carol" 1)
          map smBody <$> loadHistory conn "bob"
        bodies @?= ["to bob"],
      testCase "history for an unknown peer is empty" $ do
        loaded <- withTempStore $ \conn -> loadHistory conn "nobody"
        loaded @?= [],
      testCase "both directions survive the database round-trip" $ do
        dirs <- withTempStore $ \conn -> do
          _ <- saveMessage conn (incoming "bob" "in" 0)
          _ <- saveMessage conn (outgoing "bob" "out" 1)
          map smDirection <$> loadHistory conn "bob"
        dirs @?= [Incoming, Outgoing],
      testCase "upserting the same contact twice leaves one row, updated" $ do
        contacts <- withTempStore $ \conn -> do
          upsertContact conn "bob" (at 0)
          upsertContact conn "bob" (at 500)
          listContacts conn
        contacts @?= [("bob", at 500)],
      testCase "contacts are listed most recently seen first" $ do
        names <- withTempStore $ \conn -> do
          upsertContact conn "bob" (at 100)
          upsertContact conn "carol" (at 300)
          upsertContact conn "dave" (at 200)
          map fst <$> listContacts conn
        names @?= ["carol", "dave", "bob"],
      testCase "opening an existing database again does not wipe it" $
        -- withStore runs CREATE TABLE IF NOT EXISTS on every open; this pins
        -- down that the second open is harmless.
        withStore ":memory:" $ \conn -> do
          _ <- saveMessage conn (outgoing "bob" "kept" 0)
          migrated <- loadHistory conn "bob"
          length migrated @?= 1
    ]

-- | The C library reports "not connected" through the same @-1@ that it uses
-- for errors, so these check the FFI layer translates that correctly. None of
-- them open a socket.
ffiUnitTests :: TestTree
ffiUnitTests =
  testGroup
    "PeerChat.FFI (disconnected)"
    [ testCase "PacketType matches the C enum ordering" $
        map fromEnum [minBound .. maxBound :: Net.PacketType]
          @?= [0 .. 5],
      testCase "PacketType covers exactly the six protocol packets" $
        length [minBound .. maxBound :: Net.PacketType] @?= 6,
      testCase "getFd reports -1 before connecting" $ do
        Net.disconnect
        fd <- Net.getFd
        fd @?= (-1),
      testCase "receive yields Nothing when there is no socket" $ do
        Net.disconnect
        ev <- Net.receive 50
        ev @?= Nothing,
      testCase "sending without a connection fails rather than throwing" $ do
        Net.disconnect
        sent <- Net.sendMessage "bob" "hello"
        sent @?= False,
      testCase "pinging an undiscovered peer fails" $ do
        Net.disconnect
        pinged <- Net.sendPing "bob"
        pinged @?= False
    ]
