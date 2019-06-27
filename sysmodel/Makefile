all: hello

hello-asm: hello.s
	nasm -f elf64 -o hello.o $^
	ld -o $@ hello.o

hello: hello.c
	gcc -o $@ $^
