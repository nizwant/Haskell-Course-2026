{-# LANGUAGE OverloadedStrings #-}

-- | Property-based tests: invariants checked against randomly generated input.
--
-- Three properties, each aimed at something a single example cannot pin down:
-- that persistence is lossless, that reading one conversation reads exactly
-- that conversation, and that contact upserts are last-write-wins.
module Properties (propertyTests) where

import Data.List (sortOn)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time
import PeerChat.Store
import Test.Tasty
import Test.Tasty.QuickCheck

-- Generators ------------------------------------------------------------------

-- | Us. Every generated conversation has this user on one side.
me :: Text
me = "alice"

-- | Drawn from a small pool so that duplicates actually occur, which is the
-- interesting case for contact upserts.
genPeer :: Gen Text
genPeer = elements ["bob", "carol", "dave", "erin"]

-- | Message bodies include non-ASCII text, so the properties also cover the
-- UTF-8 path through SQLite. NUL is excluded: it terminates C strings and
-- would be lost the moment a body crossed the FFI boundary.
genBody :: Gen Text
genBody = T.pack <$> listOf (elements alphabet)
  where
    alphabet = "abcdefghij ABCDEFGHIJ 0123456789 .,!?'\"\\%_ zolc ma sie dobrze"

-- | Whole seconds only. sqlite-simple formats UTCTime with millisecond
-- precision, so finer-grained times would not survive the round-trip and the
-- property would be testing the formatter rather than the store.
genTime :: Gen UTCTime
genTime = do
  offset <- choose (0, 3000000 :: Int)
  pure (addUTCTime (fromIntegral offset) epoch)
  where
    epoch = UTCTime (fromGregorian 2026 1 1) 0

-- | A message exchanged with the given peer, in either direction.
genMessage :: Text -> Gen StoredMessage
genMessage peer = do
  direction <- elements [Incoming, Outgoing]
  body <- genBody
  timestamp <- genTime
  let (from, to) = case direction of
        Incoming -> (peer, me)
        Outgoing -> (me, peer)
  pure
    StoredMessage
      { smId = 0,
        smFrom = from,
        smTo = to,
        smBody = body,
        smTimestamp = timestamp,
        smDirection = direction
      }

-- | Everything about a message except the database-assigned id, which is the
-- one field that is deliberately not preserved from the input.
content :: StoredMessage -> (Text, Text, Text, UTCTime, Direction)
content m = (smFrom m, smTo m, smBody m, smTimestamp m, smDirection m)

-- Properties ------------------------------------------------------------------

-- | Writing a conversation and reading it back loses nothing and reorders it
-- only by timestamp.
--
-- 'sortOn' is stable and 'loadHistory' breaks timestamp ties by id, which is
-- insertion order -- so the two orderings agree even when times collide.
prop_historyRoundTrip :: Property
prop_historyRoundTrip =
  forAll (listOf (genMessage "bob")) $ \msgs -> ioProperty $ do
    loaded <- withStore ":memory:" $ \conn -> do
      mapM_ (saveMessage conn) msgs
      loadHistory conn "bob"
    pure (map content loaded === map content (sortOn smTimestamp msgs))

-- | Loading one peer's history returns every message involving that peer and
-- no others, however many conversations share the database.
prop_historyIsolatesPeers :: Property
prop_historyIsolatesPeers =
  forAll (listOf (genPeer >>= genMessage)) $ \msgs ->
    forAll genPeer $ \peer -> ioProperty $ do
      loaded <- withStore ":memory:" $ \conn -> do
        mapM_ (saveMessage conn) msgs
        loadHistory conn peer
      let expected = filter (\m -> smFrom m == peer || smTo m == peer) msgs
      pure (map content loaded === map content (sortOn smTimestamp expected))

-- | However many times a contact is upserted, it occupies exactly one row and
-- keeps the last time written for it.
prop_contactUpsertIsLastWriteWins :: Property
prop_contactUpsertIsLastWriteWins =
  forAll (listOf ((,) <$> genPeer <*> genTime)) $ \updates -> ioProperty $ do
    contacts <- withStore ":memory:" $ \conn -> do
      mapM_ (uncurry (upsertContact conn)) updates
      listContacts conn
    -- Map.fromList keeps the last value for a repeated key, which is exactly
    -- the semantics ON CONFLICT DO UPDATE is supposed to give us.
    let expected = Map.toList (Map.fromList updates)
    pure (sortOn fst contacts === expected)

propertyTests :: TestTree
propertyTests =
  testGroup
    "properties"
    [ testProperty "history survives a write/read cycle in timestamp order" prop_historyRoundTrip,
      testProperty "history contains exactly one peer's messages" prop_historyIsolatesPeers,
      testProperty "repeated contact upserts collapse to the last write" prop_contactUpsertIsLastWriteWins
    ]
