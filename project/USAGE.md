# PeerChat — running it

## Build

```sh
cd project
cabal build            # library, client, and tests
cabal test             # 37 tests; needs `cc` and a free UDP port 2137
```

The C networking library is compiled into the Haskell library by Cabal, so there is no separate
build step for it. The coordination server is the exception — it has its own `main`, so it is not
a Cabal target:

```sh
cc src/c_common.c src/server.c -o peerchat-server
```

## Run

A chat needs three processes: one coordination server, and two clients.

```sh
# terminal 1 — the server (listens on UDP 2137)
./peerchat-server

# terminal 2
PEERCHAT_SERVER_IP=127.0.0.1 cabal run PeerChat -- --user alice --password pw-alice --db alice.db

# terminal 3
PEERCHAT_SERVER_IP=127.0.0.1 cabal run PeerChat -- --user bob --password pw-bob --db bob.db
```

`--server 127.0.0.1` does the same job as the environment variable. Without either, the client
uses the address compiled into `src/c_common.h`.

| flag | meaning |
| --- | --- |
| `--user` | your username, max 31 bytes (required) |
| `--password` | claims the username; needed again to reconnect (required) |
| `--server` | coordination server IP; port is fixed at 2137 |
| `--db` | message database, default `<user>.db` |

## Using the client

| key | action |
| --- | --- |
| `Enter` | send to the selected contact |
| `↑` / `↓` | switch contact |
| `PgUp` / `PgDn` | scroll history |
| `Esc` or `Ctrl-C` | quit |

| command | action |
| --- | --- |
| `/connect <user> <password>` | look a peer up through the server |
| `/quit` | leave |

To start a conversation, one side runs `/connect`. Only one side needs to: the server tells both
peers about each other, so the other one sees the contact appear on its own.

The status bar along the bottom reports what the network layer is doing — registration, peer
lookups, and sends that failed.

## Two machines

Same thing, with the server reachable from both. Point each client at the machine running it:

```sh
cabal run PeerChat -- --user alice --password pw-alice --server 192.168.1.10
```

UDP 2137 has to be open on the server host. Behind separate NATs it will not connect — the
protocol has no hole-punching beyond what the server's address exchange provides.

## If something looks wrong

**The status bar keeps saying "announcing again".** Nothing is answering on 2137. Check the server
is running and that `--server` points at it. The client retries indefinitely and picks up on its
own once the server appears — no need to restart it.

**"could not send to X - not reachable yet".** You have not been given an address for that peer.
Run `/connect X <their password>` and wait for "X is reachable".

**A contact shows `offline` even though they are running.** Peers ping every 10 seconds and go
stale after 30, so the badge can lag by up to half a minute after they come back.

**The server prints "username is taken, wrong password".** That name is registered with a
different password. Reconnecting under a name you already hold is fine; taking someone else's is
not.

**Errors from the C layer** go to `<db>.log`, not the screen — writing them to the terminal would
paint over the interface. `tail -f alice.db.log` while it runs.

## What is where

| | |
| --- | --- |
| `src/*.c`, `src/*.h` | C networking library and coordination server |
| `src/PeerChat/FFI.hs` | bindings to it |
| `src/PeerChat/Store.hs` | SQLite persistence |
| `src/PeerChat/UI.hs` | rendering and key handling |
| `src/PeerChat/App.hs` | process wiring and the network thread |
| `test/` | unit, property, and end-to-end tests |
