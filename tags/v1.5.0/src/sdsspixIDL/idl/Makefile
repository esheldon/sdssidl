#
#
# Link with the pixel_util and create IDL applications
#
#
#
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

include ../../make.conf

CC = 		gcc
LD =            ld
INCLUDE = 	-I../include -I${IDL_DIR}/external
CCFLAGS =   ${INCLUDE} ${OS_CFLAGS} ${GSL_CFLAGS} -O6 
LDFLAGS =	${OS_LDFLAGS} ${GSL_LDFLAGS}

LDLIBS	=	-lgsl -lgslcblas -lm
#LDLIBS	=	-lm -lgsl -lgslcblas 

APPLY_PIXEL_MASK_OBJ =	applyPixelMaskIDL.o

OBJFILES = ${APPLY_PIXEL_MASK_OBJ}

SOFILES =	applyPixelMaskIDL.so
DLMS =	applyPixelMaskIDL.dlm

CP      =       cp
RM      =       rm -f

all: ${SOFILES} 
	cp -f $(SOFILES) ../../DLM
	cp -f $(DLMS) ../../DLM

applyPixelMaskIDL.so: ${APPLY_PIXEL_MASK_OBJ}
	@echo linking $@
	${LD} $< -o $@ ${LDFLAGS} ${LDLIBS} 

clean:
	- ${RM} ${SOFILES} ${OBJFILES}
	- ${RM} ../../DLM/${SOFILES}
	- ${RM} ../../DLM/${DLM}


.c.o:
	@ echo;echo compiling $<
	${CC} -c ${CCFLAGS} $<

