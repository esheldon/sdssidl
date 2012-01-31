University of Michigan/Chicago/NYU SDSS IDL libraries  v2.* 2007-03-26
---------------------------------------------------------------------


SDSSIDL

This is a set of IDL and C/C++ programs used by a group of SDSS participants,
primarily at Chicago, Michigan, and NYU but also elsewhere.  This library has a
lot of general tools that are not sdss specific as well as routines for reading
and manipulating sdss flat files and characterization of survey geometry.  The
compiled C/C++ extensions are in the form of IDL DLM interfaces, and include an
interface to the postgres database, flexible and efficient reading and writing
to binary and ascii files, an interface to Robert Lupton's atlas image reader,
an interface to Peter Kunzst' HTM code, an interface to Ryan Scranton's sdsspix
code.


INSTALLATION
------------

Untarring the file will create the directory in the current working directory.
If all you want is the IDL code and not the C/C++ extensions, just set
the environment variable

    SDSSIDL_DIR

to point to the sdssidl directory, and add the $SDSSIDL_DIR/pro to your IDL
path

If you want the C/C++ extensions: You must have IDL_DIR defined, which points
to your idl distribution, e.g. /usr/local/rsi/idl_6.2.  Enter the directory and
type

    ./configure
    make

Alternatively, if you have scons installed:

	scons

See the section below "Notes on C/C++ code" for more details


Environment variables for SDSS data
-----------------------------------

You need to have various environment variables set up to read SDSS data.
These point to directories on your file system. e.g.


    PHOTO_REDUX
    PHOTO_SWEEP
    PHOTO_RESOLVE
    PHOTO_CALIB
    BOSS_PHOTOOBJ

The most important of these are probably PHOTO_REDUX and PHOTO_SWEEP.  redux
points to the location of base data directory, under which is the imaging
and catalogs.  sweep points the the "photo sweep" directory, summary files
for the imaging that are quite handy.

For reading files, see the sdss_read function.

Notes on C/C++ code
-------------------

The configure script will check the operating system and set some
system-dependent CFLAGS and such.  Note, only the htmIndex and atlas
distributions currently support platforms other than linux, although some of
the others are simple enough to probably succeed.  If one fails, you can
always comment it out of the src/Makefile.  If you figure out the flags for
your system let us know.

If code in the htm directory does not compile because of error near "template"
in export.h, it is because you have an old version of export.h which has a
variable named template in one of the macros.  Search for "template" and change
the var name to something else (it's just a placeholder after all).

The sdsspixIDL C code depends on the Gnu Scientific library.  If this is not in
one of the usual places, it will not be compiled.

You need postgres version 8.0 or later for the compile of pgsql stuff to
succeed.

Move the directory to its final destination and set the variable SDSSIDL_DIR
equal to this directory in your .cshrc or .bashrc file.  All users should do
this.


USING EUPS
----------

You can "setup" sdssidl in the usual way.

	setup sdssidl

Or if you have a local install:
	
	setup sdssidl -r /path/to/install


SETUP FILES:

There are example setup scripts in the /setup sub-directory.  The appropriate
script (either .sh or .csh) will be sourced by the user. These files should be
copied to another location and modified to match your local settings.

Add this to your resource file:
    .cshrc:  source some_path/sdssidl_setup.csh
    .bashrc: source some_path/sdssidl_setup.sh

This will set environment variables:
    SDSSIDL_DIR  (put your local installation directory here)
    SDSSIDL_PRO_DIR (default ${SDSSIDL_DIR}/pro)
    SDSSIDL_DLM_DIR (default ${SDSSIDL_DIR}/src/DLM)
    SDSSIDL_CONFIG (default ${SDSSIDL_DIR}/config/sdssidl_setup_default.config
		   You should replace this with the location of your config
		   file. See "CONFIGURATION FILE" below.

The script will also place the SDSSIDL_PRO_DIR in the IDL_PATH, and augment the
IDL_DLM_PATH with SDSSIDL_DLM_DIR (DLM's are dynamically loadable modules. For
example, the atlas reader is a DLM written in C).  





DEPRECATED Documentation


CONFIGURATION FILE:

NOTE: The SDSSIDL package no longer makes use of the config files, instead
relying on environment variable.s

The file SDSSIDL_CONFIG (see above) will contain the default paths and
variables for sdss data on your machine. These config variables are read at run
time by the sdssidl_setup.pro script, which should be called from your
IDL_STARTUP file and is called at runtime by many routines such as
sdss_read.pro  

You should copy the default
     sdssidl_setup_default.config 
file to something that will not be over-written if you rebuild perhaps 
something including the machine name:
    sdssidl_setup_mymachine.config

Any pair of keywords-value will be added to the SDSSIDL configuration. Certain
parameters such as DATA_DIR are required for many programs, such as
read_tsobj.pro Similarly, SPEC_DIR and SPEC_VERS are required for
read_spec1d.pro The run_status_file contains info for each run/rerun on disk.
It can be created using the good_reruns.pro script.  It is used by many
programs but not the essential ones, such as read_tsobj.pro

Here is an example config file:

###########################################################################
# SDSSIDL Configuration file.  
# 
# Format:  keyword value pairs. E.g.:   
#           keyword1 value1
#           keyword2 value2
#
# Everything to the right of # comments is ignored. Also, lines that are not 
# simply two columns are ignored.
#


# SDSS data directory.  Required by many programs such as read_tsobj
data_dir       /net/cheops1/data0/imaging/

# SDSS spectro directory. If spec_dir is not specified, then
#   data_dir/spectro will be assumed. 
spec_dir	    /net/cheops1/data5/spectra/

# The version of spectro.
spec_vers      1d_23


# the run_status file holds statistics for various runs.
run_status_file     /net/cheops1/data0/run_status/sdss_run_status.fit

# Directory holding small files for efficient ra/dec searching
radec_search_dir    /net/cheops1/data0/Radec/
