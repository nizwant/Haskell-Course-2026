{-# LANGUAGE OverloadedStrings #-}

-- |
-- The terminal interface: application state, rendering, and key handling.
--
-- __Ownership.__ The event handlers here run on brick's thread, which owns the
-- SQLite connection: every 'Store' call in this module happens on that one
-- thread, because a sqlite-simple 'Store.Connection' is not thread-safe.
-- Nothing here calls into the C networking library. To send anything, a
-- handler puts a 'Command' on the queue that "PeerChat.App"'s network thread
-- drains. See that module for the other half of the arrangement.
module PeerChat.UI
  ( -- * State
    AppState (..),
    Contact (..),
    Name (..),
    AppEvent (..),
    Command (..),
    contactIsOnline,
    selectedContact,

    -- * The application
    theApp,
  )
where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Border.Style (unicode)
import Control.Concurrent.STM (TQueue, atomically, writeTQueue)
import Control.Monad (unless, when)
import Control.Monad.IO.Class (liftIO)
import Data.List (find)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time
import Graphics.Vty qualified as V
import PeerChat.FFI qualified as Net
import PeerChat.Store qualified as Store

-- | Widget names. 'Ord' is required because brick keys its viewport state on
-- them.
data Name = ContactPane | ChatPane
  deriving (Eq, Ord, Show)

-- | A peer we know about. 'cReachable' records whether the coordination server
-- has given us an address yet -- without one, sending is impossible.
data Contact = Contact
  { cName :: Text,
    cLastSeen :: Maybe UTCTime,
    cReachable :: Bool
  }
  deriving (Show)

-- | Events reaching brick from somewhere other than the keyboard.
data AppEvent
  = -- | a packet arrived, forwarded by the network thread
    NetEvent Net.Event
  | -- | once a second, so online\/offline badges go stale on their own
    Tick UTCTime
  | -- | the network thread reporting something worth showing
    NetNote Text

-- | Work for the network thread. The UI never calls into C itself.
data Command
  = -- | ask the server for a peer's address
    CmdDiscover Text Text
  | CmdSend Text Text
  | CmdPing Text
  deriving (Show)

data AppState = AppState
  { asUser :: Text,
    asStore :: Store.Connection,
    asCommands :: TQueue Command,
    asContacts :: [Contact],
    asSelected :: Int,
    -- | history of the selected conversation
    asHistory :: [Store.StoredMessage],
    asInput :: Text,
    -- | refreshed by 'Tick'; drives the online badges
    asNow :: UTCTime,
    asLastPing :: UTCTime,
    asZone :: TimeZone,
    asStatus :: Text
  }

-- | A contact counts as online if we have heard from them recently. Peers ping
-- each other every 10 seconds, so this tolerates two missed pings.
contactIsOnline :: UTCTime -> Contact -> Bool
contactIsOnline now c = case cLastSeen c of
  Nothing -> False
  Just seen -> diffUTCTime now seen < 30

selectedContact :: AppState -> Maybe Contact
selectedContact st
  | asSelected st < length (asContacts st) = Just (asContacts st !! asSelected st)
  | otherwise = Nothing

-- Rendering -------------------------------------------------------------------

drawUI :: AppState -> [Widget Name]
drawUI st = [withBorderStyle unicode (panes <=> inputBar <=> statusBar)]
  where
    panes =
      borderWithLabel (txt (" PeerChat - " <> asUser st <> " ")) $
        hBox
          [ hLimit 22 (drawContacts st),
            vBorder,
            drawChat st
          ]

    inputBar =
      vLimit 1 $
        txt "> " <+> txt (asInput st) <+> withAttr cursorAttr (txt "\9608")

    statusBar =
      vLimit 1 $
        withAttr statusAttr $
          padRight Max (txt (" " <> asStatus st))

drawContacts :: AppState -> Widget Name
drawContacts st
  | null (asContacts st) = padBottom Max (txt " no contacts yet")
  | otherwise =
      viewport ContactPane Vertical $
        vBox (zipWith row [0 ..] (asContacts st))
  where
    row i c =
      let marker = if i == asSelected st then "> " else "  "
          online = contactIsOnline (asNow st) c
          badge
            | online = withAttr onlineAttr (txt "online")
            | otherwise = withAttr offlineAttr (txt "offline")
          name = if i == asSelected st then withAttr selectedAttr (txt (cName c)) else txt (cName c)
       in txt marker <+> padRight Max name <+> badge <+> txt " "

drawChat :: AppState -> Widget Name
drawChat st = case selectedContact st of
  Nothing ->
    padBottom Max . padLeftRight 1 $
      txtWrap "No conversation selected. Use /connect <user> <password> to find a peer."
  Just c ->
    (padLeftRight 1 (txt ("Chat: " <> cName c)) <=> hBorder)
      <=> viewport ChatPane Vertical (padLeftRight 1 body)
    where
      body
        | null (asHistory st) = txtWrap "No messages yet. Type below and press Enter."
        | otherwise = vBox (map (drawMessage st) (asHistory st))

drawMessage :: AppState -> Store.StoredMessage -> Widget Name
drawMessage st m =
  txt stamp <+> withAttr who (txt (Store.smFrom m <> ": ")) <+> txtWrap (Store.smBody m)
  where
    stamp =
      T.pack $
        formatTime defaultTimeLocale "[%H:%M] " (utcToLocalTime (asZone st) (Store.smTimestamp m))
    who = case Store.smDirection m of
      Store.Outgoing -> selfAttr
      Store.Incoming -> peerAttr

-- Attributes ------------------------------------------------------------------

selectedAttr, onlineAttr, offlineAttr, selfAttr, peerAttr, statusAttr, cursorAttr :: AttrName
selectedAttr = attrName "selected"
onlineAttr = attrName "online"
offlineAttr = attrName "offline"
selfAttr = attrName "self"
peerAttr = attrName "peer"
statusAttr = attrName "status"
cursorAttr = attrName "cursor"

theMap :: AttrMap
theMap =
  attrMap
    V.defAttr
    [ (selectedAttr, V.defAttr `V.withStyle` V.bold),
      (onlineAttr, fg V.green),
      (offlineAttr, fg V.brightBlack),
      (selfAttr, fg V.cyan),
      (peerAttr, fg V.yellow),
      (statusAttr, V.black `on` V.white),
      (cursorAttr, fg V.brightBlack)
    ]

-- Event handling --------------------------------------------------------------

theApp :: App AppState AppEvent Name
theApp =
  App
    { appDraw = drawUI,
      appChooseCursor = neverShowCursor,
      appHandleEvent = handleEvent,
      appStartEvent = pure (),
      appAttrMap = const theMap
    }

-- | 'EventM' is a state monad over 'AppState', so 'get', 'modify' and friends
-- work exactly as they do in any other @State@ computation.
handleEvent :: BrickEvent Name AppEvent -> EventM Name AppState ()
handleEvent (AppEvent ev) = handleAppEvent ev
handleEvent (VtyEvent ev) = handleKey ev
handleEvent _ = pure ()

handleAppEvent :: AppEvent -> EventM Name AppState ()
handleAppEvent (NetNote note) = modify (\st -> st {asStatus = note})
handleAppEvent (Tick now) = do
  modify (\st -> st {asNow = now})
  st <- get
  -- Keep-alives are driven from here because this is the only place that knows
  -- the contact list; the network thread just executes what it is handed.
  when (diffUTCTime now (asLastPing st) >= 10) $ do
    modify (\s -> s {asLastPing = now})
    enqueue (CmdPing "server")
    mapM_ (enqueue . CmdPing . cName) (filter cReachable (asContacts st))
handleAppEvent (NetEvent ev) = case ev of
  Net.EvInitResponse ->
    modify (\st -> st {asStatus = "registered with the server"})
  Net.EvPeerDiscovered who -> do
    noteSeen who True
    modify (\st -> st {asStatus = who <> " is reachable"})
  Net.EvPing who -> noteSeen who False
  Net.EvOther _ -> pure ()
  Net.EvMessage from body -> do
    noteSeen from False
    st <- get
    now <- liftIO getCurrentTime
    saved <-
      liftIO $
        Store.saveMessage
          (asStore st)
          Store.StoredMessage
            { Store.smId = 0,
              Store.smFrom = from,
              Store.smTo = asUser st,
              Store.smBody = body,
              Store.smTimestamp = now,
              Store.smDirection = Store.Incoming
            }
    -- Only append to the visible history if this is the open conversation.
    when (fmap cName (selectedContact st) == Just from) $ do
      modify (\s -> s {asHistory = asHistory s ++ [saved]})
      vScrollToEnd (viewportScroll ChatPane)

handleKey :: V.Event -> EventM Name AppState ()
handleKey (V.EvKey V.KEsc []) = halt
handleKey (V.EvKey (V.KChar 'c') [V.MCtrl]) = halt
handleKey (V.EvKey V.KEnter []) = submit
handleKey (V.EvKey V.KBS []) = modify (\st -> st {asInput = dropEnd (asInput st)})
handleKey (V.EvKey V.KUp []) = moveSelection (-1)
handleKey (V.EvKey V.KDown []) = moveSelection 1
handleKey (V.EvKey V.KPageUp []) = vScrollBy (viewportScroll ChatPane) (-5)
handleKey (V.EvKey V.KPageDown []) = vScrollBy (viewportScroll ChatPane) 5
handleKey (V.EvKey (V.KChar c) []) = modify (\st -> st {asInput = asInput st `T.snoc` c})
handleKey _ = pure ()

dropEnd :: Text -> Text
dropEnd t = if T.null t then t else T.init t

-- | Put work on the network thread's queue.
enqueue :: Command -> EventM Name AppState ()
enqueue cmd = do
  q <- gets asCommands
  liftIO (atomically (writeTQueue q cmd))

-- | Record that we just heard from someone, creating the contact if it is new
-- and persisting the sighting.
noteSeen :: Text -> Bool -> EventM Name AppState ()
noteSeen who reachable = do
  now <- liftIO getCurrentTime
  st <- get
  liftIO (Store.upsertContact (asStore st) who now)
  let updated = case find ((== who) . cName) (asContacts st) of
        Just _ ->
          map
            ( \c ->
                if cName c == who
                  then c {cLastSeen = Just now, cReachable = cReachable c || reachable}
                  else c
            )
            (asContacts st)
        Nothing -> asContacts st ++ [Contact who (Just now) reachable]
  put st {asContacts = updated}

moveSelection :: Int -> EventM Name AppState ()
moveSelection delta = do
  st <- get
  let count = length (asContacts st)
  unless (count == 0) $ do
    let next = max 0 (min (count - 1) (asSelected st + delta))
    when (next /= asSelected st) $ do
      history <- liftIO (Store.loadHistory (asStore st) (cName (asContacts st !! next)))
      put st {asSelected = next, asHistory = history}
      vScrollToEnd (viewportScroll ChatPane)

-- | Enter was pressed: either a slash command or a message to the open chat.
submit :: EventM Name AppState ()
submit = do
  st <- get
  let input = T.strip (asInput st)
  unless (T.null input) $ do
    modify (\s -> s {asInput = ""})
    case T.words input of
      ["/connect", user, password] -> do
        enqueue (CmdDiscover user password)
        noteSeen user False
        modify (\s -> s {asStatus = "looking up " <> user <> "..."})
      ("/connect" : _) ->
        modify (\s -> s {asStatus = "usage: /connect <user> <password>"})
      ["/quit"] -> halt
      _ -> sendMessage input

sendMessage :: Text -> EventM Name AppState ()
sendMessage body = do
  st <- get
  case selectedContact st of
    Nothing ->
      modify (\s -> s {asStatus = "no contact selected - use /connect <user> <password>"})
    Just c -> do
      now <- liftIO getCurrentTime
      -- Saved before it is sent: UDP gives no delivery confirmation, so the
      -- local record is what we can actually be sure of.
      saved <-
        liftIO $
          Store.saveMessage
            (asStore st)
            Store.StoredMessage
              { Store.smId = 0,
                Store.smFrom = asUser st,
                Store.smTo = cName c,
                Store.smBody = body,
                Store.smTimestamp = now,
                Store.smDirection = Store.Outgoing
              }
      enqueue (CmdSend (cName c) body)
      modify (\s -> s {asHistory = asHistory s ++ [saved]})
      vScrollToEnd (viewportScroll ChatPane)
