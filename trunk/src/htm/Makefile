
include ../make.conf

CCXX =		g++
LD = 		g++
INCLUDE =	-I. -I${IDL_DIR}/external/include
CCFLAGS =	${INCLUDE} ${OS_CFLAGS} -O3 -Wno-deprecated
ARFLAGS = 	${OS_ARFLAGS}
CP      =       cp
RM      =       rm -f

LDFLAGS = ${OS_LDFLAGS}

# The static library
LIBNAME =	libSpatialIndex

LIBOBJS =       BitList.o\
                SpatialVector.o\
                SpatialIndex.o\
                SpatialInterface.o\
                SpatialEdge.o\
                SpatialException.o\
                SpatialConstraint.o\
                SpatialConvex.o\
                SpatialDomain.o\
                instances.o

LIBRARY = 	${LIBNAME}.a


# IDL Stuff
SOFILES = htmIndexIDL.so htmIntersectIDL.so htmMatchIDL.so
DLMFILES = htmIndexIDL.dlm htmIntersectIDL.dlm htmMatchIDL.dlm

UTIL_OBJ = htmUtilIDL.o
INDEX_IDL_OBJ = htmIndexIDL.o
INTERSECT_IDL_OBJ = htmIntersectIDL.o
MATCH_IDL_OBJ = htmMatchIDL.o

LDLIBS  = -lm


# --- Targets ----------------------------------------------------------------

# Build and install the library.

default: all
all : ${LIBRARY} ${SOFILES}
	$(CP) ${SOFILES} ${DLMFILES} ../DLM
lib : ${LIBRARY}

${LIBRARY} : ${LIBOBJS}
	@ echo linking ${LIBNAME}.a
	${AR} ${ARFLAGS} $@ $?


htmIndexIDL.so: ${INDEX_IDL_OBJ} ${UTIL_OBJ} ${LIBRARY}
	@ echo linking $@
	${LD} -o $@ ${LDFLAGS} ${INDEX_IDL_OBJ} ${UTIL_OBJ} ${LDLIBS} ${LIBRARY}

htmIntersectIDL.so: ${INTERSECT_IDL_OBJ} ${UTIL_OBJ} ${LIBRARY}
	@ echo linking $@
	${LD} -o $@ ${LDFLAGS} ${INTERSECT_IDL_OBJ} ${UTIL_OBJ} ${LDLIBS} ${LIBRARY}

htmMatchIDL.so: ${MATCH_IDL_OBJ} ${UTIL_OBJ} ${LIBRARY}
	@ echo linking $@
	${LD} -o $@ ${LDFLAGS} ${MATCH_IDL_OBJ} ${UTIL_OBJ} ${LDLIBS} ${LIBRARY}





# Clean without removing installed products.

tidy :
	- ${RM} ${LIBOBJS}
	- ${RM} ${SOFILES}
	- ${RM} *.o 

# Remove all files generated or installed by make.  

clean :	tidy
	- ${RM} ${LIBNAME}.*; rm -rf Templates.DB
	- cd ../DLM; ${RM} ${SOFILES} ${DLMFILES}


# --- Suffix Rules -----------------------------------------------------------

# First clear out the default suffixes, then declare our own and
# define the rules.

.SUFFIXES:
.SUFFIXES: .o .cpp

# Compiling C++ code.  The command lines are so long as to be useless.
# Use 'sxmake -terse' to suppress the echo of the compile command.

.cpp.o:
	@ echo compiling $<
	${CCXX} -c ${CCFLAGS} $<

