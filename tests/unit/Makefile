all: ctype env errno fds memory numbers snprintf sockets str time

%: %.c
	gcc -o $@ $< -L /build/uids -l:uids.so

.phony: clean

clean:
	rm -f args ctype env errno fds memory numbers pwd snprintf sockets str time *.o
