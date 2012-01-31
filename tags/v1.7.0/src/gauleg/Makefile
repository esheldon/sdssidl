include ../make.conf

CC	= gcc
LD	= gcc
INCLUDE	= -I${IDL_DIR}/external/include
CCFLAGS = ${INCLUDE} -O ${OS_CFLAGS}
LDFLAGS = ${OS_LDFLAGS}

LIBOBJ  = gauleg.o
LIBRARY = gauleg.so
DLM     = gauleg.dlm

all: ${LIBRARY}
	cp -f ${LIBRARY} ../DLM
	cp -f ${DLM} ../DLM

clean:
	rm -f ${LIBRARY} ${LIBOBJ}
	rm -f ../DLM/${LIBRARY}
	rm -f ../DLM/${DLM}

${LIBRARY}: ${LIBOBJ}
	${LD} ${LDFLAGS} -o $@ $?

.c.o:
	${CC} -c ${CCFLAGS} $<
