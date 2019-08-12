all: hello

hello-asm: hello.s
	nasm -f elf64 -o hello.o $^
	ld -o $@ hello.o

hello: hello.c libc.so
	gcc -c -o hello.o $^
	gcc -o $@ hello.o ./libc.so

server: server.c
	gcc -c -o server.o $^
	gcc -o $@ server.o

libc.so: libc.c
	gcc -fPIC -c $^ -o libc.o
	gcc -shared -o $@ libc.o

.PHONY: clean

clean:
	rm *.o hello server
