all: nullhttpd

# ${HOME}/uids/uids.so

CC=gcc

nullhttpd: main.o api.o ip_acl.o protocol.o version.o
	$(CC) $(CFLAGS) -o $@ $^ $(LDFLAGS)

%.o: %.c
	$(CC) $(CFLAGS) $(EXTRA_CFLAGS) -c -I. -o $@ $<

clean:
	rm -rf *.o nullhttpd
