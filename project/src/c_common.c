#include "c_common.h"

int setup_socket(int server_socket)
{
    int fd = socket(AF_INET, SOCK_DGRAM, 0);
    if (fd < 0)
    {
        perror("socket");
        return -1;
    }

    struct sockaddr_in local = {
        .sin_family = AF_INET,
        .sin_addr.s_addr = htonl(INADDR_ANY)};

    if (server_socket)
    {
        local.sin_port = htons(SERVER_PORT);
    }

    if (bind(fd, (struct sockaddr *)&local, sizeof(local)) < 0)
    {
        perror("bind");
        close(fd);
        return -1;
    }

    return fd;
}

int add_user_to_hashmap(Client **clients_hashmap, const char *username, const char *password, struct sockaddr_in src)
{
    Client *s;

    HASH_FIND_STR(*clients_hashmap, username, s);
    if (s != NULL)
    {
        return -1;
    }

    s = malloc(sizeof *s);
    snprintf(s->username, sizeof(s->username), "%s", username);
    snprintf(s->password, sizeof(s->password), "%s", password);
    s->last_time_seen = time(NULL);
    s->ip_addr = src.sin_addr;
    s->port = ntohs(src.sin_port);
    HASH_ADD_STR(*clients_hashmap, username, s);
    return 1;
}

void print_message(const struct sockaddr_in *src, const char *message)
{
    char ip[INET_ADDRSTRLEN];
    char time_str[16];

    // metadata creation
    inet_ntop(AF_INET, &src->sin_addr, ip, sizeof(ip));

    time_t now = time(NULL);
    struct tm *tm_info = localtime(&now);

    strftime(time_str, sizeof(time_str), "%H:%M", tm_info);

    PRINTF("[%s:%d; %s] %s\n",
           ip,
           ntohs(src->sin_port),
           time_str,
           message);

#ifndef USING_R
    fflush(stdout);
#endif
}
