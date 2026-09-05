#ifndef PROTOCOL_H
#define PROTOCOL_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <arpa/inet.h>
#include <sys/socket.h>
#include <sys/select.h>
#include <time.h>

#ifdef USING_R
#include <R.h>
#define PRINTF Rprintf
#define EPRINTF REprintf
#define uthash_fatal(msg) error("uthash: %s", msg)
#else
#define PRINTF printf
#define EPRINTF(...) fprintf(stderr, __VA_ARGS__)
#endif

#include "uthash.h"

#define MAX_MESS_SIZE 1024
#define SERVER_PORT 2137
#define SERVER_IP "34.28.136.103"
#define SERVER_USERNAME "server"

#ifdef DEBUG
#define DEBUG_PRINT(...) PRINTF(__VA_ARGS__)
#else
#define DEBUG_PRINT(...) ((void)0)
#endif

typedef enum _packet_type
{
    INIT,
    INIT_RESPONSE,
    PING,
    GET_PEER,
    START_PINGING_PEER,
    MESSAGE
} PacketType;

typedef struct _packet_header
{
    u_int8_t type;
    char sender_username[32];
} PacketHeader;

typedef struct _init_packet
{
    PacketHeader header;
    char password[32];
} InitPacket;

typedef struct _get_peer_packet
{
    PacketHeader header;
    char username[32];
    char password[32];
} GetPeerPacket;

typedef struct _start_pinging_peer_packet
{
    PacketHeader header;
    char username[32];
    struct in_addr ip;
    uint16_t port;
} StartPingingPeerPacket;

typedef union _metadata_packet
{
    InitPacket init;
    GetPeerPacket get_peer;
    StartPingingPeerPacket start_pinging_peer;
} MetadataPacket;

typedef struct _message_packet
{
    PacketHeader header;
    char message[MAX_MESS_SIZE];
} MessagePacket;

typedef struct _client
{
    char username[32];
    struct in_addr ip_addr;
    uint16_t port;
    time_t last_time_seen;
    char password[32];   // SHA-256 of password
    char public_key[32]; // ED25519 key used as a authentication method
    UT_hash_handle hh;
} Client;

int setup_socket(int server_socket);
int add_user_to_hashmap(Client **clients_hashmap, const char *username, const char *password, struct sockaddr_in src);
void print_message(const struct sockaddr_in *src, const char *message);

#endif
