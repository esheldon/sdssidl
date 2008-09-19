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


#if !defined (_types_hpp)
#define _types_hpp

#include <vector>
#include <map>
#include "idl_export.h"

using namespace std;

typedef struct {

  IDL_MEMINT NumTags;

  vector<string> TagNames;
  map<string,IDL_MEMINT> TagMap;
  vector<IDL_MEMINT> TagOffsets;
  vector<IDL_VPTR> TagDesc;

  IDL_MEMINT BytesPerRow;

  vector<IDL_MEMINT> TagBytes;
  vector<IDL_MEMINT> TagNelts;
  vector<string> buffer;

} TagInfoStruct;


#endif //_types_hpp
