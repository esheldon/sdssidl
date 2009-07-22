
#  Makefile for total_int.  This file is part of sdssidl
#  Copyright (C) 2005  Erin Sheldon, NYU.  erin.sheldon at gmail.com
#
#    This program is free software; you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation; either version 2 of the License, or
#    (at your option) any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with this program; if not, write to the Free Software
#    Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA

include ../make.conf

CC	= gcc
LD	= gcc
INCLUDE	= -I../include -I${IDL_DIR}/external/include
CCFLAGS = ${INCLUDE} ${OS_CFLAGS}
LDFLAGS = ${OS_LDFLAGS}

LIBOBJ  = total_int.o
LIBRARY = total_int.so
DLM		= total_int.dlm

all: ${LIBRARY}
	cp -f ${LIBRARY} ../DLM/
	cp -f ${DLM} ../DLM/

clean:
	- rm -f ${LIBRARY} ${LIBOBJ}
	- rm -f ../DLM/${LIBRARY}
	- rm -f ../DLM/${DLM}

${LIBRARY}: ${LIBOBJ}
	@ echo linking $@
	${LD} ${LDFLAGS} -o $@ $?

.c.o:
	@ echo compiling $<
	${CC} -c ${CCFLAGS} $<
