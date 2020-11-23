all: ${AUTHOR_ID}_${SERVICE_ID} ${AUTHOR_ID}_${SERVICE_ID}.so

OBJECTS := $(patsubst %.c,%.o,$(wildcard lib/*.c src/*.c))
OBJECTS := $(patsubst %.cc,%.o,$(wildcard lib/*.cc src/*.cc)) ${OBJECTS}
OBJECTS := $(patsubst %.c,%.o,$(wildcard cb_1/lib/*.c cb_1/src/*.c)) ${OBJECTS}
OBJECTS := $(patsubst %.c,%.o,$(wildcard cb_1/lib/*.cc cb_1/src/*.cc)) ${OBJECTS}

/cgc/libcgc.o: /cgc/libcgc.c
	gcc -fPIC -I /cgc/ ${CFLAGS} -c -o $@ $<

%.o: %.c
	gcc -fPIC -I /cgc/ -I ${PWD} -I ${PWD}/include -I ${PWD}/lib -I ${PWD}/cb_1/include -I ${PWD}/cb_1/lib ${CFLAGS} -c -o $@ $<

%.o: %.cc
	g++ -fPIC -I /cgc/ -I ${PWD} -I ${PWD}/include -I ${PWD}/lib -I ${PWD}/cb_1/include -I ${PWD}/cb_1/lib  ${CXXFLAGS} -c -o $@ $<

${AUTHOR_ID}_${SERVICE_ID}: ${OBJECTS} /cgc/libcgc.o
	gcc -o $@ $^ ${LDFLAGS}

${AUTHOR_ID}_${SERVICE_ID}.so: ${OBJECTS} /cgc/libcgc.o
	gcc -shared -o $@ $^ ${LDFLAGS}

.PHONY: clean

clean:
	rm -f ${PWD}/${AUTHOR_ID}_${SERVICE_ID} ${PWD}/${AUTHOR_ID}_${SERVICE_ID}.so ${PWD}/lib/*.o ${PWD}/src/*.o ${PWD}/cb_1/lib/*.o ${PWD}/cb_1/src/*.o /cgc/*.o
