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

include ../make.conf

CC = gcc
#LD =            ld
LD = gcc
INCLUDE = 	-I. -I${IDL_DIR}/external/include
CCFLAGS =   ${INCLUDE} ${OS_CFLAGS} ${GSL_CFLAGS} -O6 
LDFLAGS =	${OS_LDFLAGS} ${GSL_LDFLAGS}

LDLIBS	=	-lgsl -lgslcblas -lm
#LDLIBS	=	-lm -lgsl -lgslcblas 

APPLY_PIXEL_MASK_OBJ =	applyPixelMaskIDL.o

OBJFILES = ${APPLY_PIXEL_MASK_OBJ}

EXECUTABLES = jackknife_stripe calculate_area
EXEC_OBJS = jackknife_stripe.o calculate_area.o




SOFILES =	applyPixelMaskIDL.so
DLMS =	applyPixelMaskIDL.dlm

CP      =       cp
RM      =       rm -f

all: ${SOFILES} ${EXECUTABLES} 
	cp -f $(SOFILES) ../DLM
	cp -f $(DLMS) ../DLM

# idl extension
applyPixelMaskIDL.so: ${APPLY_PIXEL_MASK_OBJ}
	@echo linking $@
	${LD} $< -o $@ ${LDFLAGS} ${LDLIBS} 

# apps
jackknife_stripe: jackknife_stripe.o
	gcc -o jackknife_stripe $(LDLIBS) jackknife_stripe.o 
calculate_area: calculate_area.o
	gcc -o calculate_area $(LDLIBS) calculate_area.o 

clean:
	- ${RM} ${SOFILES} ${OBJFILES} ${EXEC_OBJS} ${EXECUTABLES} *.os
	- ${RM} ../DLM/${SOFILES}
	- ${RM} ../DLM/${DLMS}


.c.o:
	@ echo;echo compiling $<
	${CC} -c ${CCFLAGS} $<

