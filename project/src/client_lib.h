#ifndef CLIENT_LIB_H
#define CLIENT_LIB_H

// Connect to the server and register with the given username/password.
// Returns 0 on success, -1 on error.
int peer_connect(const char *username, const char *password);

// Re-send the INIT packet on the socket peer_connect already opened, using the
// credentials it remembered. Returns 0 on success, -1 if not connected.
//
// INIT is a single UDP datagram and the protocol has no retransmission, so one
// drop leaves a client unregistered for good: it sits in its receive loop while
// nobody can look it up. Callers should keep re-announcing until an
// INIT_RESPONSE arrives. This reuses the existing socket rather than
// reconnecting, so the port peers already know stays valid.
int peer_register(void);

// Request a peer's address from the server and store it internally.
// Returns 0 on success, -1 on error.
int peer_get_user(const char *username, const char *password);

// Send a message to a previously discovered peer (by username).
// Returns 0 on success, -1 if peer not found or send failed.
int peer_send_message(const char *recipient, const char *message);

// Send a ping to a previously discovered peer (by username).
// Returns 0 on success, -1 if peer not found or send failed.
int peer_send_ping(const char *recipient);

// Receive and process one incoming packet (non-blocking with timeout_ms).
// Returns the packet type (>=0) on success, -1 on timeout/error.
int peer_receive(int timeout_ms);

// Close the connection and free all internal state.
void peer_disconnect(void);

// Get the internal socket fd (for advanced use / select loops).
int peer_get_fd(void);

// The local UDP port the socket is bound to, or -1 when not connected.
//
// This is the address the coordination server hands out to peers, so it is
// what must stay stable across a re-announcement. Note that the descriptor
// from peer_get_fd() is not a proxy for it: closing and reopening a socket
// usually yields the same descriptor number on a different port.
int peer_get_port(void);

// Username associated with the last packet seen by peer_receive: the sender for
// MESSAGE/PING, the discovered peer for START_PINGING_PEER. Points at a static
// buffer that the next peer_receive call overwrites - copy it before reusing.
const char *peer_last_sender(void);

// Body of the last MESSAGE packet seen by peer_receive, or "" if the last
// packet was not a MESSAGE. Same static-buffer lifetime as peer_last_sender.
const char *peer_last_message(void);

#endif
