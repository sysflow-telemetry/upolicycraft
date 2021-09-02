all: ${AUTHOR_ID}_${SERVICE_ID} ${AUTHOR_ID}_${SERVICE_ID}.so lib.so

LIB_OBJECTS := $(patsubst %.c,%.o,$(wildcard lib/*.c)) $(patsubst %.cc,%.o,$(wildcard lib/*.cc))
CHALL_OBJECTS := $(patsubst %.c,%.o,$(wildcard src/*.c)) $(patsubst %.cc,%.o,$(wildcard src/*.cc))

/cgc/libcgc.so: /cgc/libcgc.c
	${CC} -shared -fPIC -I /cgc/ ${CFLAGS} -o $@ $<

%.o: %.c
	${CC} -fPIC -I /cgc/ -I ${PWD} -I ${PWD}/include -I ${PWD}/lib ${CFLAGS} -c -o $@ $<

%.o: %.cc
	g++ -fPIC -I /cgc/ -I ${PWD} -I ${PWD}/src -I ${PWD}/include -I ${PWD}/lib ${CXXFLAGS} -c -o $@ $<

lib.so: ${LIB_OBJECTS}
	${CC} -shared -o $@ $^

# ${AUTHOR_ID}_${SERVICE_ID}: ${CHALL_OBJECTS} lib.so /cgc/libcgc.so

${AUTHOR_ID}_${SERVICE_ID}: ${CHALL_OBJECTS} /cgc/libcgc.so
	${CC} -o $@ $^ ${LDFLAGS}

${AUTHOR_ID}_${SERVICE_ID}.so: ${CHALL_OBJECTS} ${LIB_OBJECTS} /cgc/libcgc.so
	${CC} -shared -o $@ $^ ${LDFLAGS}

.PHONY: clean

clean:
	rm -f ${PWD}/${AUTHOR_ID}_${SERVICE_ID} ${PWD}/${AUTHOR_ID}_${SERVICE_ID}.so ${PWD}/lib.so ${PWD}/lib/*.o ${PWD}/src/*.o /cgc/*.o
