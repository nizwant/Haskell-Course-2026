#!/usr/bin/env bash
#
# PeerChat live demo driver.
#
#   ./demo.sh              build everything and open the demo in tmux
#   ./demo.sh restart      restart Alice's client, to show history reloading
#   ./demo.sh db           dump Alice's stored messages
#   ./demo.sh stop         tear it all down
#   ./demo.sh reset        stop, and delete the demo databases
#
# Everything it creates lives in .demo/ and is safe to delete.

set -euo pipefail

SESSION=peerchat-demo
DEMO_DIR=.demo
SERVER_IP=127.0.0.1
ALICE=alice
ALICE_PW=pw-alice
BOB=bob
BOB_PW=pw-bob

# Side by side reads best on a wide screen, but each client then gets half the
# columns and the contact pane eats 22 of them, so messages start wrapping.
# Stacked gives each client the full width and half the height, which suits a
# chat far better on a projector. Auto-pick unless told otherwise.
LAYOUT=${LAYOUT:-auto}

cd "$(dirname "$0")"

die() {
  echo "demo: $*" >&2
  exit 1
}

require() {
  command -v "$1" >/dev/null 2>&1 || die "$1 is not installed"
}

# The server owns a fixed port, so an instance left over from a previous demo
# would silently keep the new one from binding.
free_port() {
  local holders
  holders=$(lsof -ti udp:2137 2>/dev/null || true)
  if [ -n "$holders" ]; then
    echo "demo: freeing UDP 2137"
    echo "$holders" | xargs kill 2>/dev/null || true
    sleep 1
  fi
}

client_cmd() {
  # $1 user, $2 password, $3 db
  printf 'cd %q && TERM=xterm-256color %q --user %q --password %q --server %q --db %q' \
    "$PWD/$DEMO_DIR" "$CLIENT" "$1" "$2" "$SERVER_IP" "$3"
}

start() {
  require tmux
  require cc
  require cabal

  [ -f PeerChat.cabal ] || die "run this from the project directory"

  echo "demo: building..."
  cabal build exe:PeerChat >/dev/null
  CLIENT=$(cabal list-bin PeerChat)

  mkdir -p "$DEMO_DIR"
  cc src/c_common.c src/server.c -o "$DEMO_DIR/peerchat-server"

  tmux kill-session -t "$SESSION" 2>/dev/null || true
  free_port

  # Server first, in its own window so it does not compete for space with the
  # clients. The log is worth showing: every registration and lookup appears.
  tmux new-session -d -s "$SESSION" -n server \
    "cd $PWD/$DEMO_DIR && ./peerchat-server"
  sleep 1

  local split columns
  columns=$(tput cols 2>/dev/null || echo 80)
  case "$LAYOUT" in
    side) split=-h ;;
    stacked) split=-v ;;
    # A client needs roughly 75 columns before messages stop wrapping awkwardly.
    auto) if [ "$columns" -ge 150 ]; then split=-h; else split=-v; fi ;;
    *) die "LAYOUT must be auto, side, or stacked" ;;
  esac

  # Capture pane ids rather than indices: tmux numbers panes from 0 or 1
  # depending on the user's pane-base-index, but ids (%3, %4...) are stable.
  local alice_pane
  alice_pane=$(tmux new-window -P -F '#{pane_id}' -t "$SESSION" -n chat \
    "$(client_cmd "$ALICE" "$ALICE_PW" alice.db)")
  tmux split-window "$split" -P -F '#{pane_id}' -t "$alice_pane" \
    "$(client_cmd "$BOB" "$BOB_PW" bob.db)" >/dev/null
  tmux select-pane -t "$alice_pane"
  echo "$alice_pane" > "$DEMO_DIR/alice.pane"

  cat <<EOF

  PeerChat demo ready.        attach with:  tmux attach -t $SESSION

  $ALICE and $BOB, laid out $([ "$split" = "-h" ] && echo "side by side" || echo "stacked ($ALICE on top)")
  re-run with  LAYOUT=side ./demo.sh  or  LAYOUT=stacked ./demo.sh  to change that
  switch panes   Ctrl-b o     pick a window   Ctrl-b w     detach   Ctrl-b d

  Suggested run of play:
    1. in $ALICE:  /connect $BOB $BOB_PW      -> "$BOB is reachable", badge turns online
    2. in $ALICE:  type a message, Enter      -> it appears in $BOB's pane
    3. in $BOB:    reply                      -> it appears in $ALICE's pane
    4. detach (Ctrl-b d), then:  ./demo.sh restart
       $ALICE restarts and reloads the whole conversation from SQLite.
       This is the persistence beat.
    5. in $ALICE:  /connect $BOB $BOB_PW      -> both go online again
       $ALICE came back on a NEW port, so $BOB was holding a dead address for
       her. The server hands out the new one and $BOB adopts it. Before that
       fix she stayed unreachable until the server itself restarted.
    6. ./demo.sh db                           -> the same messages, straight from the database

EOF
}

restart_alice() {
  tmux has-session -t "$SESSION" 2>/dev/null || die "no demo running"
  [ -f "$DEMO_DIR/alice.pane" ] || die "no demo pane recorded; run ./demo.sh first"
  CLIENT=$(cabal list-bin PeerChat)
  # -k kills what is in the pane first, so this is a genuine client restart:
  # new process, new socket, history read back from disk.
  tmux respawn-pane -k -t "$(cat "$DEMO_DIR/alice.pane")" \
    "$(client_cmd "$ALICE" "$ALICE_PW" alice.db)"
  echo "demo: $ALICE restarted -- reattach to see the history reload"
}

dump_db() {
  local db="$DEMO_DIR/alice.db"
  [ -f "$db" ] || die "no database yet at $db"
  require sqlite3
  echo "-- messages in $db"
  sqlite3 -header -column "$db" \
    "SELECT id, from_user, to_user, direction, body FROM messages ORDER BY timestamp, id;"
  echo
  echo "-- contacts"
  sqlite3 -header -column "$db" "SELECT username, last_seen FROM contacts;"
}

stop() {
  tmux kill-session -t "$SESSION" 2>/dev/null || true
  free_port
  echo "demo: stopped"
}

case "${1:-start}" in
  start) start ;;
  restart) restart_alice ;;
  db) dump_db ;;
  stop) stop ;;
  reset)
    stop
    rm -f "$DEMO_DIR"/*.db "$DEMO_DIR"/*.log "$DEMO_DIR"/*.pane
    echo "demo: databases cleared"
    ;;
  *) die "unknown command: $1 (start, restart, db, stop, reset)" ;;
esac
