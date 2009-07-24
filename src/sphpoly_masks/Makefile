include ../make.conf

CC	= gcc
LD	= ld

INCLUDE = -I../include -I${IDL_DIR}/external
CCFLAGS = ${INCLUDE} ${OS_CFLAGS} -O3
LDFLAGS = ${OS_LDFLAGS}

# local shared and object files
SPHPOLYCOMP_SOFILE	= sphPolyCompIDL.so

OBJS	= mask.o sphPolyCompIDL.o

SOFILES	= ${SPHPOLYCOMP_SOFILE}

LDLIBS	= -lm

DLMS = sphPolyCompIDL.dlm

# how to build each shared object file

all: ${SOFILES}
	cp -f $(SOFILES) ../DLM
	cp -f $(DLMS) ../DLM

${SPHPOLYCOMP_SOFILE}: ${OBJS}
	@ echo linking $@
	${LD} ${LDFLAGS} ${LDLIBS} -o $@ ${OBJS}


clean:
	- rm -f ${SOFILES}; 
	- rm -f ${OBJS}; 
	- rm -f ../DLM/${SPHPOLYCOMP_SOFILE}
	- rm -f ../DLM/${DLMS}

.SUFFIXES:
.SUFFIXES: .o .c

.c.o:
	@ echo; echo compiling $<
	${CC} -c ${CCFLAGS} $<
