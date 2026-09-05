#include "c_common.h"
#include "client_lib.h"

static Client *clients_hashmap = NULL;
static int client_fd = -1;
static struct sockaddr_in server_addr;

int peer_connect(const char *username, const char *password)
{
    client_fd = setup_socket(0);
    if (client_fd < 0)
    {
        return -1;
    }
    DEBUG_PRINT("Socket created (fd=%d)\n", client_fd);

    server_addr.sin_family = AF_INET;
    server_addr.sin_port = htons(SERVER_PORT);

    const char *server_ip = getenv("PEERCHAT_SERVER_IP");
    if (!server_ip)
        server_ip = SERVER_IP;

    if (inet_pton(AF_INET, server_ip, &server_addr.sin_addr) != 1)
    {
        EPRINTF("bad server IP\n");
        close(client_fd);
        client_fd = -1;
        return -1;
    }

    // add self to hashmap
    struct sockaddr_in self_addr = {
        .sin_family = AF_INET,
        .sin_addr.s_addr = htonl(INADDR_ANY)};
    add_user_to_hashmap(&clients_hashmap, username, password, self_addr);

    // add server to hashmap
    add_user_to_hashmap(&clients_hashmap, SERVER_USERNAME, SERVER_USERNAME, server_addr);

    // send INIT packet
    PacketHeader header = {0};
    header.type = INIT;
    strncpy(header.sender_username, username, sizeof(header.sender_username) - 1);

    InitPacket init_packet = {0};
    init_packet.header = header;
    strncpy(init_packet.password, password, sizeof(init_packet.password) - 1);

    if (sendto(client_fd, &init_packet, sizeof(init_packet), 0,
               (struct sockaddr *)&server_addr, sizeof(server_addr)) < 0)
    {
        perror("sendto");
        return -1;
    }

    DEBUG_PRINT("INIT packet sent as '%s'\n", username);
    return 0;
}

int peer_get_user(const char *username, const char *password)
{
    if (client_fd < 0)
        return -1;

    PacketHeader header = {0};
    header.type = GET_PEER;

    GetPeerPacket p = {0};
    p.header = header;
    strncpy(p.username, username, sizeof(p.username) - 1);
    strncpy(p.password, password, sizeof(p.password) - 1);

    if (sendto(client_fd, &p, sizeof(p), 0,
               (struct sockaddr *)&server_addr, sizeof(server_addr)) < 0)
    {
        perror("sendto");
        return -1;
    }
    DEBUG_PRINT("GET_PEER request sent for '%s'\n", username);
    return 0;
}

int peer_send_message(const char *recipient, const char *message)
{
    if (client_fd < 0)
        return -1;

    Client *peer;
    HASH_FIND_STR(clients_hashmap, recipient, peer);
    if (peer == NULL)
    {
        EPRINTF("peer '%s' not found, use peer_get_user first\n", recipient);
        return -1;
    }

    struct sockaddr_in dest = {0};
    dest.sin_family = AF_INET;
    dest.sin_port = htons(peer->port);
    dest.sin_addr = peer->ip_addr;

    PacketHeader header = {0};
    header.type = MESSAGE;

    MessagePacket p = {0};
    p.header = header;
    strncpy(p.message, message, sizeof(p.message) - 1);
    p.message[sizeof(p.message) - 1] = '\0';

    if (sendto(client_fd, &p, sizeof(p), 0,
               (struct sockaddr *)&dest, sizeof(dest)) < 0)
    {
        perror("sendto");
        return -1;
    }
    DEBUG_PRINT("MESSAGE sent to '%s'\n", recipient);
    return 0;
}

int peer_send_ping(const char *recipient)
{
    if (client_fd < 0)
        return -1;

    Client *peer;
    HASH_FIND_STR(clients_hashmap, recipient, peer);
    if (peer == NULL)
    {
        EPRINTF("peer '%s' not found, use peer_get_user first\n", recipient);
        return -1;
    }

    struct sockaddr_in dest = {0};
    dest.sin_family = AF_INET;
    dest.sin_port = htons(peer->port);
    dest.sin_addr = peer->ip_addr;

    PacketHeader header = {0};
    header.type = PING;

    if (sendto(client_fd, &header, sizeof(header), 0,
               (struct sockaddr *)&dest, sizeof(dest)) < 0)
    {
        perror("sendto");
        return -1;
    }
    DEBUG_PRINT("PING sent to '%s'\n", recipient);
    return 0;
}

int peer_receive(int timeout_ms)
{
    if (client_fd < 0)
        return -1;

    fd_set rfds;
    FD_ZERO(&rfds);
    FD_SET(client_fd, &rfds);

    struct timeval tv;
    tv.tv_sec = timeout_ms / 1000;
    tv.tv_usec = (timeout_ms % 1000) * 1000;

    int ret = select(client_fd + 1, &rfds, NULL, NULL, &tv);
    if (ret <= 0)
        return -1;

    char buf[sizeof(MessagePacket)];
    struct sockaddr_in src;
    socklen_t slen = sizeof(src);

    ssize_t n = recvfrom(client_fd, buf, sizeof(buf) - 1, 0,
                         (struct sockaddr *)&src, &slen);
    if (n < 0)
    {
        perror("recvfrom");
        return -1;
    }

    PacketHeader *header = (PacketHeader *)buf;

    switch (header->type)
    {
    case INIT_RESPONSE:
        DEBUG_PRINT("INIT_RESPONSE Packet received\n");
        break;

    case PING:
        DEBUG_PRINT("PING Packet received\n");
        break;

    case START_PINGING_PEER:
    {
        StartPingingPeerPacket *spp = (StartPingingPeerPacket *)buf;
        DEBUG_PRINT("START_PINGING_PEER Packet received\n");
        DEBUG_PRINT("Username: %s\n", spp->username);
        DEBUG_PRINT("Port: %d\n", spp->port);

        struct sockaddr_in peer_addr = {0};
        peer_addr.sin_family = AF_INET;
        peer_addr.sin_port = htons(spp->port);
        peer_addr.sin_addr = spp->ip;

        add_user_to_hashmap(&clients_hashmap, spp->username, "", peer_addr);

        char peer_ip[INET_ADDRSTRLEN];
        inet_ntop(AF_INET, &spp->ip, peer_ip, sizeof(peer_ip));
        DEBUG_PRINT("Peer %s added (%s:%d)\n", spp->username, peer_ip, spp->port);
        break;
    }

    case MESSAGE:
    {
        MessagePacket *packet = (MessagePacket *)buf;
        char received_message[MAX_MESS_SIZE];
        strncpy(received_message, packet->message, sizeof(received_message) - 1);
        received_message[sizeof(received_message) - 1] = '\0';

        print_message(&src, received_message);
        break;
    }

    default:
        DEBUG_PRINT("Unknown packet type: %d\n", header->type);
        break;
    }

    return header->type;
}

void peer_disconnect(void)
{
    if (client_fd >= 0)
    {
        close(client_fd);
        DEBUG_PRINT("Socket closed (fd=%d)\n", client_fd);
        client_fd = -1;
    }

    Client *current, *tmp;
    HASH_ITER(hh, clients_hashmap, current, tmp)
    {
        HASH_DEL(clients_hashmap, current);
        free(current);
    }
    clients_hashmap = NULL;
}

int peer_get_fd(void)
{
    return client_fd;
}
