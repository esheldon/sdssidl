#  This file is part of pgsql_query, an IDL DLM interface to the postgreSQL
# database.  
#
#  Copyright (C) 2005  Erin Sheldon, NYU.  erin dot sheldon at gmail dot com
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

CC = gcc
LD = gcc
#LD = ld

# Will include /usr/local/pgsql.  This is actually the default 
# install place from the tar ball.

INCLUDE = -I${IDL_DIR}/external/include
CFLAGS = ${INCLUDE} ${OS_CFLAGS} ${PG_CFLAGS}
#CFLAGS = ${INCLUDE} ${PG_CFLAGS}
LDFLAGS = ${OS_LDFLAGS} ${PG_LDFLAGS}
#LDFLAGS = ${PG_LDFLAGS}

LDLIBS = -lpq -lz 

SOFILES = pgsql_query.so 
OBJFILES = pgsql_query.o pgsql_query_util.o 
DLMS = pgsql_query.dlm 

CP = cp -f

default: all

all: ${SOFILES} ${DLM}
	${CP} ${SOFILES} ../DLM
	${CP} ${DLMS} ../DLM

clean:
	- ${RM} ${SOFILES} ${OBJFILES}
	- ${RM} ../DLM/${SOFILES} ../DLM/${DLMS}

pgsql_query.so: pgsql_query.o pgsql_query_util.o
	@ echo linking $@
	${LD} -o $@ pgsql_query.o pgsql_query_util.o ${LDFLAGS} ${LDLIBS}


pgsql_query.o: pgsql_query.c pgsql_query.h
pgsql_query_util.o: pgsql_query_util.c pgsql_query.h
