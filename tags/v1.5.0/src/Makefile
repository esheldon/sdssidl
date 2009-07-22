#  This file is part of the sdssidl IDL routines. 
#  Makefile for sdssidl
#
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

#
# Build routines in the SDSSIDL C/C++ library
#

# This will define the CODE_DIRS variable used below
include ./codedirs

default : all

# Deal with any special files for these operating systems

linux:
	cd atlas; ${MAKE} $@
	cd htmIndexIDL; ${MAKE} $@

osf:
	cd atlas; ${MAKE} $@
	cd htmIndexIDL; ${MAKE} $@

irix:
	cd atlas; ${MAKE} $@

# Note we will attempt to make all directories even if it is untested on 
# a particular operating system.

DIVIDER="------------------------------------------------------------------------"
all clean:
	@ for d in ${CODE_DIRS}; do \
		(cd $$d; echo ${DIVIDER}; echo;echo Making $$d; ${MAKE} $@); \
	done

