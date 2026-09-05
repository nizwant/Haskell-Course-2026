#ifndef CLIENT_LIB_H
#define CLIENT_LIB_H

// Connect to the server and register with the given username/password.
// Returns 0 on success, -1 on error.
int peer_connect(const char *username, const char *password);

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

#endif
