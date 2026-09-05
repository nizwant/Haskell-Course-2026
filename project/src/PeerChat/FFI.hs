-- |
-- Bindings to the C networking library in @src/client_lib.h@.
--
-- Intended to be imported qualified:
--
-- > import PeerChat.FFI qualified as Net
--
-- __Threading.__ The C library keeps its socket and peer table in static
-- globals, so it is not thread-safe and there is exactly one connection per
-- process. Every function here must be called from a single dedicated Haskell
-- thread. In the finished client that is the thread running the 'receive'
-- loop; other threads ask it to do things over a channel rather than calling
-- into C themselves.
--
-- __Truncation.__ The C protocol uses fixed-size fields: usernames and
-- passwords are cut to 31 bytes and message bodies to 1023 bytes, silently.
module PeerChat.FFI
  ( -- * Connection
    connect,
    disconnect,
    getFd,

    -- * Sending
    getUser,
    sendMessage,
    sendPing,

    -- * Receiving
    Event (..),
    PacketType (..),
    receive,
  )
where

import Data.ByteString qualified as BS
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import Data.Text.Encoding.Error (lenientDecode)
import Foreign.C.String (CString)
import Foreign.C.Types (CInt (..))

-- | Protocol packet types, mirroring the C @PacketType@ enum. The derived
-- 'Enum' instance must stay in the same order as the enum in @c_common.h@.
data PacketType
  = Init
  | InitResponse
  | Ping
  | GetPeer
  | StartPingingPeer
  | Message
  deriving (Show, Eq, Enum, Bounded)

-- | A decoded incoming packet. 'receive' returns one of these instead of a
-- bare packet type so that the message body is copied out of the C static
-- buffer before anything else can overwrite it.
data Event
  = -- | sender, body
    EvMessage Text Text
  | -- | a peer's address arrived from the server; it is now reachable
    EvPeerDiscovered Text
  | -- | keep-alive from a peer
    EvPing Text
  | -- | the server acknowledged our registration
    EvInitResponse
  | -- | a packet a client should not normally receive
    EvOther PacketType
  deriving (Show, Eq)

-- Foreign imports -----------------------------------------------------------
--
-- Anything that touches the socket is imported @safe@: a @safe@ call releases
-- the capability while it runs, so other Haskell threads keep going. This is
-- essential for peer_receive, which blocks in select() for up to timeout_ms --
-- an @unsafe@ import there would freeze the entire runtime, TUI included.
-- Only the plain accessors, which just read a static and return, are @unsafe@.

foreign import ccall safe "client_lib.h peer_connect"
  c_peer_connect :: CString -> CString -> IO CInt

foreign import ccall safe "client_lib.h peer_get_user"
  c_peer_get_user :: CString -> CString -> IO CInt

foreign import ccall safe "client_lib.h peer_send_message"
  c_peer_send_message :: CString -> CString -> IO CInt

foreign import ccall safe "client_lib.h peer_send_ping"
  c_peer_send_ping :: CString -> IO CInt

foreign import ccall safe "client_lib.h peer_receive"
  c_peer_receive :: CInt -> IO CInt

foreign import ccall safe "client_lib.h peer_disconnect"
  c_peer_disconnect :: IO ()

foreign import ccall unsafe "client_lib.h peer_get_fd"
  c_peer_get_fd :: IO CInt

foreign import ccall unsafe "client_lib.h peer_last_sender"
  c_peer_last_sender :: IO CString

foreign import ccall unsafe "client_lib.h peer_last_message"
  c_peer_last_message :: IO CString

-- Marshalling helpers -------------------------------------------------------

-- | Hand a 'Text' to C as a NUL-terminated UTF-8 string, valid only for the
-- duration of the callback.
withText :: Text -> (CString -> IO a) -> IO a
withText = BS.useAsCString . TE.encodeUtf8

-- | Copy a NUL-terminated C string into a 'Text'. Invalid UTF-8 is replaced
-- rather than throwing, since the bytes come off the network.
peekText :: CString -> IO Text
peekText cs = TE.decodeUtf8With lenientDecode <$> BS.packCString cs

-- | The C functions all report success as @0@ and failure as @-1@, with no
-- further detail available.
succeeded :: CInt -> Bool
succeeded = (== 0)

-- API -----------------------------------------------------------------------

-- | Create the socket and register with the coordination server. Returns
-- 'False' if the socket could not be opened or the server address was bad.
--
-- Note that this only /sends/ the INIT packet; the server's acknowledgement
-- arrives later as an 'EvInitResponse' from 'receive'.
connect :: Text -> Text -> IO Bool
connect user password =
  withText user $ \u ->
    withText password $ \p ->
      succeeded <$> c_peer_connect u p

-- | Ask the server for a peer's address. Like 'connect' this is fire-and-
-- forget: success here means the request was sent, and the answer arrives
-- later as an 'EvPeerDiscovered'.
getUser :: Text -> Text -> IO Bool
getUser user password =
  withText user $ \u ->
    withText password $ \p ->
      succeeded <$> c_peer_get_user u p

-- | Send a message to a peer discovered earlier via 'getUser'. Returns 'False'
-- if that peer's address is not known yet.
sendMessage :: Text -> Text -> IO Bool
sendMessage recipient body =
  withText recipient $ \r ->
    withText body $ \b ->
      succeeded <$> c_peer_send_message r b

-- | Send a keep-alive ping to a known peer.
sendPing :: Text -> IO Bool
sendPing recipient =
  withText recipient $ \r ->
    succeeded <$> c_peer_send_ping r

-- | Wait up to the given number of milliseconds for one incoming packet.
-- Returns 'Nothing' on timeout or error.
receive :: Int -> IO (Maybe Event)
receive timeoutMs = do
  n <- c_peer_receive (fromIntegral timeoutMs)
  traverse readEvent (packetTypeFromC n)

-- | Read the details of the packet that 'receive' just processed. Must run
-- before any further call into the C library, which would overwrite them.
readEvent :: PacketType -> IO Event
readEvent pt = do
  who <- peekText =<< c_peer_last_sender
  case pt of
    Message -> EvMessage who <$> (peekText =<< c_peer_last_message)
    StartPingingPeer -> pure (EvPeerDiscovered who)
    Ping -> pure (EvPing who)
    InitResponse -> pure EvInitResponse
    other -> pure (EvOther other)

-- | @peer_receive@ returns a packet type or @-1@; anything outside the enum
-- range is treated as no event.
packetTypeFromC :: CInt -> Maybe PacketType
packetTypeFromC n
  | n >= lo && n <= hi = Just (toEnum (fromIntegral n))
  | otherwise = Nothing
  where
    lo = fromIntegral (fromEnum (minBound :: PacketType))
    hi = fromIntegral (fromEnum (maxBound :: PacketType))

-- | Close the socket and free the peer table. Safe to call more than once.
disconnect :: IO ()
disconnect = c_peer_disconnect

-- | The underlying UDP socket descriptor, or @-1@ when not connected.
getFd :: IO Int
getFd = fromIntegral <$> c_peer_get_fd
