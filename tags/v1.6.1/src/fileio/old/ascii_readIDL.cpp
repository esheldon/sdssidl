/*---------------------------------------------------------------------------
  NAME:
    ascii_read

  CALLING SEQUENCE:
    IDL> struct = ascii_read(file/lun, structdef, numrows, 
                             rows=, columns=, skiplines=, /csv, status=, verbose=, /help)
  
  PURPOSE: 

    Read ASCII data from file into a structure.  The file can be white space or
    comma separated value (CSV). The structdef input provides the definition
    describing each row of the file. Particular rows or columns may be
    extracted by number. For very large files, this is a big advantage over the
    IDL readf procedure which can only read contiguous chunks.  The return
    variable is a structure containing the requested data.

    Unlike binary_read, the columns may be variable length and the user can
    input the string columns in structdef with any size because the memory will
    be generated on the fly.  E.g. structdef = {a:0L, b:'', c:0LL}. String
    columns are currently limited to 255 bytes.

    Either the file name or an IDL file unit may be sent.  When a file unit is
    sent, the must be opened with the /stdio keyword and bufsize=0. Lines can
    be skipped using the skiplines= keyword.

    In general, due to the complex inputs and the fact that most files will
    have a header describing the data, this program will be used as a utility
    program and an IDL wrapper will parse the header and format the structure
    definition.

    This program is written in C++ and is linked to IDL via the DLM mechanism.


  INPUTS: 
     file/lun: Filename or file unit. For string file names, the user must 
               expand all ~ or other environment variables.  If the file
	       unit is entered, the file must be opened with the appropriate 
	       keywords:
                 openr, lun, file, /get_lun, /stdio, bufsize=0

     structdef: A structure that describes the layout of the data in each row.
                Variable length fields are not supported.
     numrows: Number of rows in the file.

  OPTIONAL INPUTS:
     rows=: An array or scalar of unique rows to read
     skiplines=: The number of lines to skip.  The newline character is searched
         for, so be careful.  This is useful if there is a text header but not
	 well defined otherwise.
     columns=: An array or scalar of unique columns numbers to extract.
     /csv: The file is formatted as comma separated value.  The fields cannot 
           contain commas in this case.
     verbose=: 0 for standard quiet. 1 for basic info. > 1 for debug mode.
     /help: print this message.

  OPTIONAL OUTPUTS:
    status=: The status of the read. 0 for success, 1 for read failure, 
             2 for input errors such as bad file unit.

  REVISION HISTORY:
    created 20-April-2006: Erin Sheldon, NYU
    Converted to C++, 2006-July-17, E.S. NYU


  Copyright (C) 2005  Erin Sheldon, NYU.  erin dot sheldon at gmail dot com

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA


 
  ---------------------------------------------------------------------------*/


#include <iostream>
#include <cstdio>
#include <vector>
#include <string>

#include "idl_export.h"
#include "FileObj.hpp"

using namespace std;


static IDL_VPTR ascii_read(int argc, IDL_VPTR argv[], char argk[])
{



static IDL_KW_PAR kw_pars[] = {

  IDL_KW_FAST_SCAN, 

  /* These must be in alphabetical order */

  /* 
     IDL_KW_VIN: can be just an input, like var=3
     IDL_KW_OUT: can be output variable, var=var
     IDL_KW_ZERO: If not there, variable is set to 0

     If the VIN is set, then the _there variables will be
     0 when an undefined name variable is sent: no good for
     returning variables!
  */

  {"APPEND", 
   IDL_TYP_LONG, 
   1, 
   0, 
   static_cast<int *> IDL_KW_OFFSETOF(append_there), 
   static_cast<char *> IDL_KW_OFFSETOF(append) },

  {"BRACKET_ARRAYS", 
   IDL_TYP_LONG, 
   1, 
   0, 
   static_cast<int *> IDL_KW_OFFSETOF(bracket_arrays_there), 
   static_cast<char *> IDL_KW_OFFSETOF(bracket_arrays) },



  /* Column and row numbers to read */
  {"COLUMNS", 
   IDL_TYP_UNDEF, 
   1, 
   IDL_KW_VIN, 
   static_cast<int *> IDL_KW_OFFSETOF(columns_there), 
   static_cast<char *> IDL_KW_OFFSETOF(columns)},

  // Ignored for binary
  {"CSV", 
   IDL_TYP_LONG, 
   1, 
   0, 
   static_cast<int *> IDL_KW_OFFSETOF(csv_there), 
   static_cast<char *> IDL_KW_OFFSETOF(csv) },

  {"DELIMITER", 
   IDL_TYP_STRING, 
   1, 
   0, 
   static_cast<int *> IDL_KW_OFFSETOF(delimiter_there), 
   static_cast<char * >IDL_KW_OFFSETOF(delimiter) },


  {"HELP", 
   IDL_TYP_LONG, 
   1, 
   0, 
   static_cast<int *> IDL_KW_OFFSETOF(help_there), 
   static_cast<char *> IDL_KW_OFFSETOF(help) },

  {"ROWS", 
   IDL_TYP_UNDEF, 
   1, 
   IDL_KW_VIN, 
   static_cast<int *> IDL_KW_OFFSETOF(rows_there), 
   static_cast<char *> IDL_KW_OFFSETOF(rows)},


  {"SKIPLINES", 
   IDL_TYP_MEMINT, 
   1, 
   0, 
   static_cast<int *> IDL_KW_OFFSETOF(skiplines_there), 
   static_cast<char *> IDL_KW_OFFSETOF(skiplines) },


  {"STATUS", 
   IDL_TYP_UNDEF, 
   1, 
   IDL_KW_OUT | IDL_KW_ZERO, 
   static_cast<int *> IDL_KW_OFFSETOF(status_there), 
   static_cast<char *> IDL_KW_OFFSETOF(status) },

  // There was some kind of memory problem associated with this
  // when this was declared as IDL_TYP_INT but the keyword value
  // above was int
  {"VERBOSE", 
   IDL_TYP_LONG, 
   1, 
   0, 
   static_cast<int *> IDL_KW_OFFSETOF(verbose_there), 
   static_cast<char *> IDL_KW_OFFSETOF(verbose) },
  
  
  { NULL }
};



KW_RESULT kw;




    // Process the keywords. kw_pars defined in keywords.hpp
    int npars = IDL_KWProcessByOffset(argc, argv, argk, kw_pars, 
            (IDL_VPTR *) 0, 1, &kw);

    FileObj fileobj(npars, argv, argk, &kw, ACTION_READ_ASCII);

    // Syntax errors or help requests are special cases
    if ( FILEOBJ_SYNTAX_ERROR == fileobj.Status() || 
            FILEOBJ_PRINT_HELP   == fileobj.Status() )
    {
        fileobj.AsciiReadSyntax();
        return(IDL_GettmpInt(-1));
    }

    // Other errors
    if (fileobj.Status() != FILEOBJ_OK)
        return(IDL_GettmpInt(-1));   


    // Read the file as ASCII
    //if (!fileobj.ReadAsAscii())
    //    return(IDL_GettmpInt(-1));   
    if (!fileobj.ReadFile())
        return(IDL_GettmpInt(-1));   

    // We have a result.
    IDL_VPTR resultV = fileobj.OutputStruct();

    IDL_KW_FREE;
    return(resultV);

}
































//////////////////////////////////////////////////////////////////////////////
// This defines the IDL_Load function used for Dynamically Loadable Modules
// It includes a fix for the name mangling done by g++
//////////////////////////////////////////////////////////////////////////////

#define ARRLEN(arr) (sizeof(arr)/sizeof(arr[0]))

/*
 * Here's the code to fix the name mangling of g++
 */

//
// First is the name twist of the original function
//
int IDL_Load_(void);

//
// Next are the shells declared with "external C"
//
extern "C" {
  int IDL_Load(void);
}

//
// Last are the one-line functions to call the originals
//
int IDL_Load(void) {
  return(IDL_Load_());
}

int IDL_Load_(void)
{

  /* This must be static. It is a struct. */
  /* The name in strings is the name by which it will be called from IDL and
     MUST BE CAPITALIZED 
     5th parameter will say if it accepts keywords and some other flags 
     For more info see page 325 of external dev. guide */
  static IDL_SYSFUN_DEF2 ascii_read_addr[] = {
    { (IDL_SYSRTN_GENERIC) ascii_read, 
      "ASCII_READ", 
      0, 
      IDL_MAXPARAMS, 
      IDL_SYSFUN_DEF_F_KEYWORDS, 0},
  };

  /* The false means it is not a function */
  return IDL_SysRtnAdd(ascii_read_addr, IDL_TRUE, ARRLEN(ascii_read_addr));

}

