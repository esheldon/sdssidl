#	PACKAGE:	SpatialIndex
#
# --- Macros -----------------------------------------------------------------

# os dependent stuff in the config
include ../../make.conf

CCXX =		g++
LD = 		g++
INCLUDE =	-I../include -I${IDL_DIR}/external
CCFLAGS =	${INCLUDE} ${OS_CFLAGS} -O3 -Wno-deprecated
LDFLAGS = ${OS_LDFLAGS}
CP      =       cp -f
RM      =       rm -f

LIBRARY =	../lib/libSpatialIndex.a
SOFILES = htmIndexIDL.so htmIntersectIDL.so htmMatchIDL.so

UTIL_OBJ = htmUtilIDL.o
INDEX_IDL_OBJ = htmIndexIDL.o
INTERSECT_IDL_OBJ = htmIntersectIDL.o
MATCH_IDL_OBJ = htmMatchIDL.o

LDLIBS  = -lm

default: all
all: ${SOFILES}
	$(CP) ${SOFILES} ../../DLM


htmIndexIDL.so: ${INDEX_IDL_OBJ} ${UTIL_OBJ} ${LIBRARY}
	@ echo linking $@
	${LD} -o $@ ${LDFLAGS} ${INDEX_IDL_OBJ} ${UTIL_OBJ} ${LDLIBS} ${LIBRARY}

htmIntersectIDL.so: ${INTERSECT_IDL_OBJ} ${UTIL_OBJ} ${LIBRARY}
	@ echo linking $@
	${LD} -o $@ ${LDFLAGS} ${INTERSECT_IDL_OBJ} ${UTIL_OBJ} ${LDLIBS} ${LIBRARY}

htmMatchIDL.so: ${MATCH_IDL_OBJ} ${UTIL_OBJ} ${LIBRARY}
	@ echo linking $@
	${LD} -o $@ ${LDFLAGS} ${MATCH_IDL_OBJ} ${UTIL_OBJ} ${LDLIBS} ${LIBRARY}

clean:
	- ${RM} *.o *.so
	- cd ../../DLM; ${RM} ${SOFILES}

.SUFFIXES:
.SUFFIXES: .o .cpp

.cpp.o:
	@ echo compiling $<
	${CCXX} -c ${CCFLAGS} $<
