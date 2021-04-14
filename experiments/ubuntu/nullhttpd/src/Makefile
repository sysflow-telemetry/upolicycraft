all: nullhttpd

nullhttpd: main.o api.o ip_acl.o protocol.o version.o ${HOME}/uids/uids.so
	$(CC) $(LDFLAGS) -o $@ $^

%.o: %.c
	$(CC) $(CFLAGS) $(EXTRA_CFLAGS) -c -I. -o $@ $<

clean:
	rm -rf *.o nullhttpd
