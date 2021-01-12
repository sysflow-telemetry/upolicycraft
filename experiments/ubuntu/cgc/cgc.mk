all: ${AUTHOR_ID}_${SERVICE_ID} ${AUTHOR_ID}_${SERVICE_ID}.so lib.so

LIB_OBJECTS := $(patsubst %.c,%.o,$(wildcard lib/*.c lib/*.cc))
CHALL_OBJECTS := $(patsubst %.c,%.o,$(wildcard src/*.c src/*.cc))

/cgc/libcgc.so: /cgc/libcgc.c
	gcc -shared -fPIC -I /cgc/ ${CFLAGS} -o $@ $<

%.o: %.c
	gcc -fPIC -I /cgc/ -I ${PWD} -I ${PWD}/include -I ${PWD}/lib -I ${PWD}/cb_1/include -I ${PWD}/cb_1/lib ${CFLAGS} -c -o $@ $<

%.o: %.cc
	g++ -fPIC -I /cgc/ -I ${PWD} -I ${PWD}/include -I ${PWD}/lib -I ${PWD}/cb_1/include -I ${PWD}/cb_1/lib  ${CXXFLAGS} -c -o $@ $<

lib.so: ${LIB_OBJECTS}
	gcc -shared -o $@ $^

${AUTHOR_ID}_${SERVICE_ID}: ${CHALL_OBJECTS} lib.so /cgc/libcgc.so
	gcc -o $@ $^ ${LDFLAGS}

${AUTHOR_ID}_${SERVICE_ID}.so: ${CHALL_OBJECTS}
	gcc -shared -o $@ $^ ${LDFLAGS}

.PHONY: clean

clean:
	rm -f ${PWD}/${AUTHOR_ID}_${SERVICE_ID} ${PWD}/${AUTHOR_ID}_${SERVICE_ID}.so ${PWD}/lib/*.o ${PWD}/src/*.o ${PWD}/cb_1/lib/*.o ${PWD}/cb_1/src/*.o /cgc/*.o
