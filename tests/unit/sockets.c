#include <stdio.h>
#include <stddef.h>
/* According to POSIX.1-2001, POSIX.1-2008 */
#include <sys/select.h>

/* According to earlier standards */
#include <sys/stat.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/socket.h>

#include <unistd.h>
#include <fcntl.h>

#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>

extern uids_log(char *);
extern uids_debug(void *);

int main(int argc, char *argv[]) {
    int sockets[2];

    socketpair(AF_UNIX, SOCK_STREAM, 0, sockets);
    uids_debug(sockets[0]);
    uids_debug(sockets[1]);
    close(sockets[0]);
    close(sockets[1]);

    printf("sa_family offset: %d %d %d\n", offsetof(struct sockaddr, sa_family), AF_INET, AF_INET6);
    printf("sa_data offset: %d\n", offsetof(struct sockaddr, sa_data));

    int server_fd = socket(AF_INET, SOCK_STREAM, 0);
    struct sockaddr_in the_addr;
    int socklen;

    int retval = getpeername(server_fd, &the_addr, &socklen);
    printf("%d\n", retval);
    printf("Sizeof the_addr: %d\n", socklen);
    printf("Offset of sin_addr: %d\n", offsetof(struct sockaddr_in, sin_addr));
    printf("Sizeof of sin_addr: %d\n", sizeof(the_addr.sin_addr));

    struct in_addr sin_addr;

    inet_aton("127.0.0.1", &sin_addr);
    printf("127.0.0.1 = %lx\n", sin_addr); 
}
