# PeerChat: A persistent peer-to-peer messaging system with a terminal UI

## Motivation

Real-time messaging systems like Telegram, Signal, and WhatsApp are some of the most widely used software on the planet. Under the hood they combine networking protocols, binary serialization, local storage, and responsive user interfaces into a cohesive experience. Most tutorials stop at "echo server" — as a separate project I have built a complete UDP peer-to-peer chat system in C ([github.com/nizwant/computational_data_science/tree/main/project](https://github.com/nizwant/computational_data_science/tree/main/project)) that implements socket management, a binary packet protocol with multiple packet types, peer discovery through a coordination server, keep-alive pings, and a `uthash`-based user registry. That finished C codebase provides a proven networking layer; what it lacks is message persistence, a proper user interface, and a type-safe application layer — exactly the things Haskell excels at.

## Project Overview

PeerChat wraps the C networking library with Haskell through the Foreign Function Interface, then layers on two things the C code does not provide: SQLite-backed local message storage and a `brick`-based terminal UI. The C library is responsible for UDP sockets, packet encode/decode, peer discovery, and keep-alive pings. Haskell owns the application logic, persistence, and presentation. A user launches the client, registers with the coordination server, looks up a peer by username, and begins chatting — with every message saved locally so history survives restarts.

## Key Goals

1. **C Networking Library + Haskell FFI Bindings**: A reusable C library encapsulating the UDP protocol (connection setup, packet encode/decode, peer discovery, keep-alive) exposed to Haskell via the Foreign Function Interface.
2. **TUI Client + SQLite Persistence**: A `brick`-based terminal UI backed by `sqlite-simple` for local message storage, providing scrollable history, contact status, and an interactive chat experience.
3. **Test Suite**: Unit tests, end-to-end tests, and property-based tests covering the FFI boundary, persistence layer, and full message-exchange scenarios.
4. **Encryption** (optional stretch goal): Add NaCl/libsodium-based end-to-end encryption between peers, with public keys registered at the coordination server to prevent MITM attacks.

## Suggested Core Data Types

```haskell
-- These types are a starting point; their shapes will evolve during implementation.

-- | Protocol packet types, mirroring the C enum
data PacketType
  = Init
  | InitResponse
  | Ping
  | GetPeer
  | StartPingingPeer
  | Message
  deriving (Show, Eq, Enum, Bounded)

-- | High-level Haskell representation of a protocol packet
data Packet
  = PktInit        { pktSender :: Text, pktPassword :: Text }
  | PktInitResp    { pktSender :: Text }
  | PktPing        { pktSender :: Text }
  | PktGetPeer     { pktSender :: Text, pktTarget :: Text, pktTargetPass :: Text }
  | PktStartPeer   { pktSender :: Text, pktPeerUser :: Text, pktPeerAddr :: SockAddr }
  | PktMessage     { pktSender :: Text, pktBody :: Text }
  deriving (Show, Eq)

-- | A message stored in the local SQLite database
data StoredMessage = StoredMessage
  { smId        :: Int64
  , smFrom      :: Text
  , smTo        :: Text
  , smBody      :: Text
  , smTimestamp  :: UTCTime
  , smDirection  :: Direction
  } deriving (Show, Eq)

data Direction = Incoming | Outgoing deriving (Show, Eq)

-- | Online/offline status of a known contact
data ContactStatus = Online | Offline UTCTime deriving (Show, Eq)

-- | The top-level TUI application state
data AppState = AppState
  { asCurrentUser  :: Text
  , asContacts     :: Map Text ContactStatus
  , asActiveChat   :: Maybe Text
  , asMessages     :: [StoredMessage]       -- currently displayed conversation
  , asInputBuffer  :: Text
  , asServerConn   :: ForeignPtr Connection  -- opaque handle to C connection state
  }
```

## Example

A typical session (TUI output simplified for readability):

```
$ peerchat --user alice --password s3cret --server 192.168.1.10:2137

┌─ PeerChat ─────────────────────────────────────────────────┐
│ Contacts          │  Chat: bob                             │
│                   │                                        │
│ > bob    [online] │  [14:02] bob: hey, did you finish the  │
│   carol [offline] │              lab yet?                  │
│                   │  [14:03] alice: almost, just the tests │
│                   │  [14:05] bob: nice, want to compare?   │
│                   │  [14:05] alice: sure, give me 10 min   │
│                   │                                        │
│                   │                                        │
├───────────────────┴────────────────────────────────────────┤
│ > yeah let me push first█                                  │
└────────────────────────────────────────────────────────────┘

# After restarting the client, previous messages are still visible
# (loaded from local SQLite database).

$ peerchat --user alice --password s3cret --server 192.168.1.10:2137

┌─ PeerChat ─────────────────────────────────────────────────┐
│ Contacts          │  Chat: bob                             │
│                   │                                        │
│ > bob   [offline] │  [14:02] bob: hey, did you finish the  │
│   carol [offline] │              lab yet?                  │
│                   │  [14:03] alice: almost, just the tests │
│                   │  [14:05] bob: nice, want to compare?   │
│                   │  [14:05] alice: sure, give me 10 min   │
│                   │  [14:06] alice: yeah let me push first │
│                   │                                        │
├───────────────────┴────────────────────────────────────────┤
│ > █                                                        │
└────────────────────────────────────────────────────────────┘
```

## Implementation Components

### 1. C Networking Library + Haskell FFI Bindings

- **Package the completed C chat codebase as a library** (`libpeerchat`): the UDP socket setup, packet serialization/deserialization, and hash-map-based user registry (finished and tested in the standalone C project) are exposed through a clean C API with an opaque `Connection` handle.
- **Haskell FFI module** (`PeerChat.FFI`): use `foreign import ccall` to bind every C function, marshal Haskell `Text`/`ByteString` to C strings, manage `Connection` lifetime with `ForeignPtr` and C finalizers.
- **Haskell protocol layer** (`PeerChat.Protocol`): convert raw C events into the typed `Packet` ADT; provide `encodePacket` / `decodePacket` as a pure Haskell layer on top of the FFI.

### 2. TUI Client + SQLite Persistence

- **SQLite persistence** (`PeerChat.Store`):
  - Schema: `messages(id, from_user, to_user, body, timestamp, direction)`, `contacts(username, last_seen)`.
  - Functions: `saveMessage`, `loadHistory`, `upsertContact`, `listContacts`.
  - Use `sqlite-simple` with parameterized queries to prevent injection.
- **TUI** (`PeerChat.UI`):
  - Built with `brick` and `vty`.
  - Left pane: contact list with online/offline badges; right pane: scrollable message history for the selected contact; bottom bar: text input.
  - Keyboard navigation: `Tab` to switch panes, `Enter` to send, `↑/↓` to scroll history, `/connect <user> <pass>` to initiate a peer connection.
- **Event loop glue** (`PeerChat.App`):
  - Spawn a background thread that calls `pc_poll` in a loop and pushes `BChan` events into `brick`.
  - Another background thread sends `pc_ping` every 10 seconds.
  - On incoming message: save to SQLite, update UI. On outgoing message: call `pc_send`, save to SQLite, update UI.
  - On peer status change: update contact list.

### 3. Test Suite

- **Unit tests** (`test/Unit.hs`):
  - `PeerChat.Protocol`: `decodePacket . encodePacket ≡ id` for each packet variant.
  - `PeerChat.Store`: insert a message → query it back → fields match. Upsert a contact twice → only one row exists.
  - `PeerChat.FFI`: marshal a Haskell `Text` to C and back → identical.
- **End-to-end tests** (`test/E2E.hs`):
  - Start a real coordination server process, start two clients (Alice and Bob), have Alice discover Bob, exchange three messages each way, shut down Alice's client, restart it, verify all six messages are present in the SQLite database.
  - Test offline scenario: Alice sends while Bob is disconnected → message is saved locally but delivery status reflects non-delivery.
- **Property-based tests** (`test/Properties.hs`):
  - **Serialization round-trip**: `∀ p :: Packet. decodePacket (encodePacket p) ≡ Right p` — generated with `Arbitrary` instances for every `Packet` constructor.
  - **Persistence round-trip**: `∀ ms :: [StoredMessage]. loadHistory (saveAll ms) ≡ sort ms` — messages survive a write→read cycle and come back in timestamp order.
  - **Username uniqueness**: registering the same username twice at the server always results in exactly one entry (tested by scripting rapid concurrent registrations).
