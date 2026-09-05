{-# LANGUAGE OverloadedStrings #-}

-- |
-- Local SQLite storage for messages and known contacts, so that history
-- survives a restart.
--
-- Intended to be imported qualified:
--
-- > import PeerChat.Store qualified as Store
--
-- Each client owns one database file, so the tables never need to record who
-- "we" are -- every row is a conversation between us and one peer.
--
-- All queries are parameterised: user-supplied text is passed as a bound value
-- and never spliced into SQL.
module PeerChat.Store
  ( -- * Opening
    withStore,

    -- * Messages
    StoredMessage (..),
    Direction (..),
    saveMessage,
    loadHistory,

    -- * Contacts
    upsertContact,
    listContacts,
  )
where

import Control.Exception (bracket)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Time (UTCTime)
import Database.SQLite.Simple
import Database.SQLite.Simple.FromField (FromField (..), returnError)
import Database.SQLite.Simple.ToField (ToField (..))

-- | Whether a message was received from a peer or sent by us.
data Direction = Incoming | Outgoing
  deriving (Show, Eq, Ord)

-- | A message as it lives in the database.
data StoredMessage = StoredMessage
  { smId :: Int64,
    smFrom :: Text,
    smTo :: Text,
    smBody :: Text,
    smTimestamp :: UTCTime,
    smDirection :: Direction
  }
  deriving (Show, Eq)

-- Storing a Direction ---------------------------------------------------------
--
-- Stored as the text 'in'/'out' rather than 0/1, so that the database stays
-- readable from the sqlite3 shell while debugging.

instance ToField Direction where
  toField Incoming = SQLText "in"
  toField Outgoing = SQLText "out"

instance FromField Direction where
  fromField f = do
    -- Ok is a monad, so we can reuse Text's own parser and then narrow it.
    t <- fromField f
    case t :: Text of
      "in" -> pure Incoming
      "out" -> pure Outgoing
      _ -> returnError ConversionFailed f "expected 'in' or 'out'"

-- | Column order here must match the SELECTs below and the INSERT in
-- 'saveMessage'.
instance FromRow StoredMessage where
  fromRow = StoredMessage <$> field <*> field <*> field <*> field <*> field <*> field

-- Opening ---------------------------------------------------------------------

-- | Open the database at the given path, create the tables if this is the
-- first run, and hand the connection to the callback. The connection is closed
-- afterwards even if the callback throws.
withStore :: FilePath -> (Connection -> IO a) -> IO a
withStore path action =
  bracket (open path) close $ \conn -> do
    migrate conn
    action conn

migrate :: Connection -> IO ()
migrate conn = do
  execute_
    conn
    "CREATE TABLE IF NOT EXISTS messages \
    \( id        INTEGER PRIMARY KEY \
    \, from_user TEXT NOT NULL \
    \, to_user   TEXT NOT NULL \
    \, body      TEXT NOT NULL \
    \, timestamp TEXT NOT NULL \
    \, direction TEXT NOT NULL )"

  -- Looking up one conversation in timestamp order is the only read the UI
  -- makes, so index exactly that.
  execute_
    conn
    "CREATE INDEX IF NOT EXISTS messages_by_time ON messages (timestamp)"

  -- username is the primary key, which is what makes upsertContact's
  -- ON CONFLICT clause work.
  execute_
    conn
    "CREATE TABLE IF NOT EXISTS contacts \
    \( username  TEXT PRIMARY KEY \
    \, last_seen TEXT NOT NULL )"

-- Messages --------------------------------------------------------------------

-- | Insert a message. The 'smId' of the argument is ignored: SQLite assigns
-- the id, and the returned message carries it.
saveMessage :: Connection -> StoredMessage -> IO StoredMessage
saveMessage conn msg = do
  execute
    conn
    "INSERT INTO messages (from_user, to_user, body, timestamp, direction) \
    \VALUES (?, ?, ?, ?, ?)"
    (smFrom msg, smTo msg, smBody msg, smTimestamp msg, smDirection msg)
  newId <- lastInsertRowId conn
  pure msg {smId = newId}

-- | Every message exchanged with the given peer, oldest first.
loadHistory :: Connection -> Text -> IO [StoredMessage]
loadHistory conn peer =
  query
    conn
    "SELECT id, from_user, to_user, body, timestamp, direction FROM messages \
    \WHERE from_user = ? OR to_user = ? \
    \ORDER BY timestamp, id"
    (peer, peer)

-- Contacts --------------------------------------------------------------------

-- | Record that a contact exists and when it was last seen. Calling this twice
-- for the same username updates the existing row rather than adding another.
upsertContact :: Connection -> Text -> UTCTime -> IO ()
upsertContact conn username lastSeen =
  execute
    conn
    "INSERT INTO contacts (username, last_seen) VALUES (?, ?) \
    \ON CONFLICT (username) DO UPDATE SET last_seen = excluded.last_seen"
    (username, lastSeen)

-- | All known contacts with the time each was last seen, most recent first.
-- Online/offline is a runtime property, so the caller decides that; the
-- database only knows when someone was last heard from.
listContacts :: Connection -> IO [(Text, UTCTime)]
listContacts conn =
  query_ conn "SELECT username, last_seen FROM contacts ORDER BY last_seen DESC"
