#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <unistd.h>

#define FILESIZE 0x100
#define BUFFER_SIZE 0x10
#define on_error(...) { fprintf(stderr, __VA_ARGS__); exit(1); }

typedef struct {
  char buf[BUFFER_SIZE];
  ssize_t (*read)(int, struct msghdr *, int);
  ssize_t (*write)(int, const struct msghdr *, int);
} Server;

int main (int argc, char *argv[]) {
  char buf[FILESIZE];
  char c;
  char *p = buf;
  char *port_label = "port: ";

  if (argc < 2) on_error("Usage: %s [config]\n", argv[0]);

  FILE *fp = fopen("/echo/echo.conf", "r");

  if (fp == NULL) {
    on_error("Could not open config file");
  }

  while ((c = fgetc(fp)) != EOF && p < &buf[FILESIZE]) {
    *p++ = c;
  }

  fclose(fp);

  if (memcmp(buf, port_label, strlen(port_label)) != 0) {
    on_error("Invalid config file!");
  }

  int port;

  port = atoi(buf+strlen(port_label));

  printf("Running on port %d\n", port);

  int server_fd, client_fd, err;
  struct sockaddr_in server, client;
  Server serve = {{'\0'}, recvmsg, sendmsg};

  server_fd = socket(AF_INET, SOCK_STREAM, 0);
  if (server_fd < 0) on_error("Could not create socket\n");

  server.sin_family = AF_INET;
  server.sin_port = htons(port);
  server.sin_addr.s_addr = htonl(INADDR_ANY);

  int opt_val = 1;
  setsockopt(server_fd, SOL_SOCKET, SO_REUSEADDR, &opt_val, sizeof opt_val);

  struct iovec iov[1];
  struct msghdr  msg;

  memset(&msg,   0, sizeof(msg));
  memset(iov,    0, sizeof(iov));

  iov[0].iov_base = serve.buf;
  iov[0].iov_len  = sizeof(serve.buf);
  msg.msg_iov     = iov;
  msg.msg_iovlen  = 1;

  err = bind(server_fd, (struct sockaddr *)&server, sizeof(server));
  if (err < 0) on_error("Could not bind socket\n");

  err = listen(server_fd, 128);
  if (err < 0) on_error("Could not listen on socket\n");

  while (1) {
    socklen_t client_len = sizeof(client);
    client_fd = accept(server_fd, (struct sockaddr *) &client, &client_len);

    if (client_fd < 0) on_error("Could not establish new connection\n");

    while (1) {
      /** Mistakenly trust user input */
      int recvd = serve.read(client_fd, &msg, 0);

      if (recvd < 0) on_error("Error reading.\n");

      serve.buf[recvd] = '\0';

      int read = atoi(serve.buf);

      iov[0].iov_len = read;
      serve.read(client_fd, &msg, 0);
      serve.write(client_fd, &msg, 0);
    }
  }

  return 0;
}
