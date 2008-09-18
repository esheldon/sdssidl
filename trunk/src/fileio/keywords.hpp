/*
   Header used by FileObj.cpp

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


#if !defined (_keywords_hpp)
#define _keywords_hpp

#include "idl_export.h"

using namespace std;

typedef struct {
  IDL_KW_RESULT_FIRST_FIELD; // Must be first entry in structure

  // These must be in alpabetical order

  // Ignored for reading
  IDL_LONG append;
  int append_there;

  // Ignored for reading
  IDL_LONG bracket_arrays;
  int bracket_arrays_there;


  IDL_VPTR columns;
  int columns_there;

  // Ignored for reading
  IDL_STRING delimiter;
  int delimiter_there;

  // ignored for binary
  IDL_LONG csv;
  int csv_there;

  IDL_LONG help;
  int help_there;

  IDL_VPTR rows;
  int rows_there;

  // Lines to skip
  IDL_MEMINT skiplines;
  int skiplines_there;

  // Status of the call 
  IDL_VPTR status;
  int status_there;

  // ignored for binary
  IDL_LONG tab;
  int tab_there;

  IDL_LONG verbose;
  int verbose_there;

  // ignored for binary
  IDL_LONG whitespace;
  int whitespace_there;

  
} KW_RESULT;


#endif // _keywords_hpp
