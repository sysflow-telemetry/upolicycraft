#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <unistd.h>

#define BUFFER_SIZE 0x10
#define on_error(...) { fprintf(stderr, __VA_ARGS__); fflush(stderr); exit(1); }

typedef struct {
  char buf[BUFFER_SIZE];
  ssize_t (*read)(int, void *, size_t, int);
  ssize_t (*write)(int, void *, size_t, int);
} Server;

int main (int argc, char *argv[]) {
  if (argc < 2) on_error("Usage: %s [port]\n", argv[0]);

  int port = atoi(argv[1]);

  int server_fd, client_fd, err;
  struct sockaddr_in server, client;
  Server serve = {{'\0'}, recv, send};

  server_fd = socket(AF_INET, SOCK_STREAM, 0);
  if (server_fd < 0) on_error("Could not create socket\n");

  server.sin_family = AF_INET;
  server.sin_port = htons(port);
  server.sin_addr.s_addr = htonl(INADDR_ANY);

  int opt_val = 1;
  setsockopt(server_fd, SOL_SOCKET, SO_REUSEADDR, &opt_val, sizeof opt_val);

  err = bind(server_fd, (struct sockaddr *)&server, sizeof(server));
  if (err < 0) on_error("Could not bind socket\n");

  err = listen(server_fd, 128);
  if (err < 0) on_error("Could not listen on socket\n");

  printf("Server is listening on %d\n", port);

  while (1) {
    socklen_t client_len = sizeof(client);
    client_fd = accept(server_fd, (struct sockaddr *) &client, &client_len);

    if (client_fd < 0) on_error("Could not establish new connection\n");

    while (1) {
      /** Mistakenly trust user input. */
      int recvd = serve.read(client_fd, serve.buf, 10, 0);

      if (recvd < 0) on_error("Error reading.\n");

      serve.buf[recvd] = '\0';

      printf("Buffer = %s\n", serve.buf);

      int read = atoi(serve.buf);

      serve.read(client_fd, serve.buf, read, 0);
      serve.write(client_fd, serve.buf, read, 0);
    }
  }

  return 0;
}
