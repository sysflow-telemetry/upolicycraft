all: ${AUTHOR_ID}_${SERVICE_ID}

OBJECTS := $(patsubst %.c,%.o,$(wildcard lib/*.c src/*.c))

%.o: %.c
	gcc -I /cgc/ -I ${PWD} -I ${PWD}/include -I ${PWD}/lib ${CFLAGS} -c -o $@ $<

${AUTHOR_ID}_${SERVICE_ID}: ${OBJECTS} /cgc/libcgc.o
	gcc -o $@ $^

.PHONY: clean

clean:
	rm -f ${PWD}/lib/*.o ${PWD}/src/*.o /cgc/*.o
