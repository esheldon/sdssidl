# SDSS Science Archive Package/include Makefile:
#
#	PACKAGE:	SpatialIndex
#

# --- Macros -----------------------------------------------------------------

# For the 'html' target.  All include files to be installed in the
# doc/www directory for this package:

HTMLDOC =	SpatialConstraint.h\
		SpatialConvex.h\
		SpatialDomain.h\
		SpatialIndex.h\
		SpatialInterface.h\
		SpatialVector.h\
		SpatialException.h\
		BitList.h\
		VarVec.h\
		VarStr.h\
		SpatialDoc.h

SHELL = ${MAKESHELL}

DOCXX_FLAGS = -H -b -d ../doc/www -B ../etc/copyright.html

# --- Targets ----------------------------------------------------------------

default: default_message

# Make online documentation, anchored to this directory, for each of
# the files specified in HTML_CXX.  

all html : ${HTMLDOC:.h=.dxx}
	  @echo Making Documentation...
	  ${TERSE} ${DOCXX_DIR}/doc++ ${DOCXX_FLAGS} ${HTMLDOC:.h=.dxx} > dxx 2>&1
	  ${TERSE} ${RM} ${HTMLDOC:.h=.dxx} dxx

clean :
	- cd ../doc/www; ${RM} *.html .html *.class *.tex gifs.db icon*.gif down.gif logo.gif;

# Naive attempt to force use of sxmake.  See 'default' target above.

default_message	:
	@ echo ############################################################
	@ echo
	@ echo ERROR: Invalid make request.
	@ echo
	@ echo Please invoke this makefile using sxmake.
	@ echo
	@ echo ############################################################
	@ echo

# Functionality of 'install' target is reserved for future use.

install	:
	@ echo ############################################################
	@ echo
	@ echo ERROR: Invalid make request.
	@ echo
	@ echo To install an SDSS Science Archive Package, you must check
	@ echo the package out in the install directory and make all.
	@ echo
	@ echo ############################################################
	@ echo

# --- Suffix Rules -----------------------------------------------------------

# First clear out the default suffixes, then declare our own and
# define the rules.

.SUFFIXES:
.SUFFIXES: .h .dxx

# generating doc files

.h.dxx:
	${TERSE} ${DOCXX_DIR}/docify $*.h $*.d
	${TERSE} sed -e 's/LINKAGE //g' $*.d > $*.dxx
	${TERSE} rm -f $*.d

