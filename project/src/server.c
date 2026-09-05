#include "c_common.h"

int main()
{

    // initialize hashmap
    Client *clients_hashmap = NULL;

    // add server to hashmap

    struct sockaddr_in server_socket = {
        .sin_family = AF_INET,
        .sin_addr.s_addr = htonl(INADDR_ANY),
        .sin_port = htons(SERVER_PORT)};

    add_user_to_hashmap(&clients_hashmap, SERVER_USERNAME, "very_hard_password", server_socket);

    int fd = setup_socket(1);
    if (fd < 0)
    {
        return 1;
    }

    printf("Listening on UDP port %d...\n", SERVER_PORT);

    while (1)
    {
        fd_set rfds;
        FD_ZERO(&rfds);
        FD_SET(fd, &rfds);

        if (select(fd + 1, &rfds, NULL, NULL, NULL) < 0)
        {
            perror("select");
            break;
        }

        if (FD_ISSET(fd, &rfds))
        {
            char buf[sizeof(MetadataPacket)];
            struct sockaddr_in src;
            socklen_t slen = sizeof(src);

            ssize_t n = recvfrom(fd, buf, sizeof(buf) - 1, 0, (struct sockaddr *)&src, &slen);
            if (n < 0)
            {
                perror("recvfrom");
                continue;
            }

            PacketHeader *header = (PacketHeader *)buf;
            int type = header->type;
            char username[32];
            snprintf(username, sizeof(username), "%s", header->sender_username);

            switch (type)
            {
            case INIT:
                printf("INIT Packet received\n");

                InitPacket *init_packet = (InitPacket *)buf;
                char password[32];
                strncpy(password, init_packet->password, sizeof(password) - 1);
                password[sizeof(password) - 1] = '\0';

                if (add_user_to_hashmap(&clients_hashmap, username, password, src) < 0)
                {
                    printf("user already exist\n");
                    break;
                }

                PacketHeader init_r = {INIT_RESPONSE, SERVER_USERNAME};
                if (sendto(fd, &init_r, sizeof(init_r), 0, (struct sockaddr *)&src, sizeof(src)) < 0)
                {
                    perror("sendto");
                }
                break;

            case PING:
                printf("PING Packet received\n");
                Client *s;

                HASH_FIND_STR(clients_hashmap, username, s);
                if (s == NULL)
                {
                    printf("user that PINGed doesn't exist in hashmap\n");
                    break;
                }
                s->last_time_seen = time(NULL);
                break;

            case GET_PEER:
                printf("GET_PEER Packet received\n");

                GetPeerPacket *packet = (GetPeerPacket *)buf;
                char dest_username[32];
                char dest_password[32];
                strncpy(dest_username, packet->username, sizeof(dest_username) - 1);
                strncpy(dest_password, packet->password, sizeof(dest_password) - 1);

                Client *sb;
                HASH_FIND_STR(clients_hashmap, dest_username, sb);
                if (sb == NULL)
                {
                    printf("GET_PEER user doesn't exist in hashmap\n");
                    break;
                }

                if (strcmp(sb->password, dest_password) != 0)
                {
                    printf("Password doesn't match\n");
                    break;
                }

                // send info to requester
                PacketHeader start_pinging_r = {START_PINGING_PEER, SERVER_USERNAME};

                // create StartPingingPeerPacket
                StartPingingPeerPacket start_packet_source = {0};
                start_packet_source.header = start_pinging_r;
                strncpy(start_packet_source.username, dest_username, sizeof(start_packet_source.username) - 1);
                start_packet_source.ip = sb->ip_addr;
                start_packet_source.port = sb->port;

                if (sendto(fd, &start_packet_source, sizeof(start_packet_source), 0, (struct sockaddr *)&src, sizeof(src)) < 0)
                {
                    perror("sendto");
                }

                // send START_PINGING_PEER to requested

                struct sockaddr_in peer = {0};
                peer.sin_family = AF_INET;
                peer.sin_port = htons(sb->port);
                peer.sin_addr = sb->ip_addr;

                // create StartPingingPeerPacket
                StartPingingPeerPacket start_packet_dest = {0};
                start_packet_dest.header = start_pinging_r;
                strncpy(start_packet_dest.username, username, sizeof(start_packet_dest.username) - 1);
                start_packet_dest.ip = src.sin_addr;
                start_packet_dest.port = ntohs(src.sin_port);

                if (sendto(fd, &start_packet_dest, sizeof(start_packet_dest), 0, (struct sockaddr *)&peer, sizeof(peer)) < 0)
                {
                    perror("sendto");
                }

                break;

            default:
                printf("Unknown packet type for server: %d\n", type);
                break;
            }
        }
    }

    close(fd);
    return 0;
}
