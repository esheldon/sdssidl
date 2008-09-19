

/*

  This is to be called from IDL as 
     sum = total_int(ARRAY or SCALAR)
     
  It is like the built in total() function, but works with integers.  Total
  converts everything to float prior to IDL 6; now supports the /integer
  keyword.


  This program is part of sdssidl.
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

*/


#include <stdio.h>
#include "idl_export.h"
IDL_VPTR total_int(int argc, IDL_VPTR argv[])
{

    IDL_VPTR src;
    IDL_VPTR result;
    IDL_LONG64 *src_d;

    IDL_LONG64 sum=0;

    short src_is_copy=0;

    int n;
    IDL_MEMINT n_elements;

    if (argc != 1)
    {
        IDL_Message(IDL_M_NAMED_GENERIC, IDL_MSG_LONGJMP,
                "Syntax: sum = total_int(ARRAY or SCALAR)");
    }

    /* Point src to the input IDL_VPTR */
    src = argv[0];

    /* ensure its a simple variable */
    IDL_ENSURE_SIMPLE(src);

    /* make sure it is of type IDL_LONG64 
       If not, we will make a copy with that type
       better way is outlined in the rsum3 example */
    if (src->type != IDL_TYP_LONG64)
    {
        src_is_copy=1;
        src = IDL_CvtLng64(1, &argv[0]);
    }

    /* This will automatically deal with scalar versus array input */
    IDL_VarGetData(src, &n_elements, (char **) &src_d, FALSE);

    for (n=n_elements;n--;)
        sum += *src_d++;

    result = IDL_Gettmp();

    result->type = IDL_TYP_LONG64;
    result->value.l64 = sum;


    if (src_is_copy)
        IDL_Deltmp(src);

    return(result);


}

#define ARRLEN(arr) (sizeof(arr)/sizeof(arr[0]))

int IDL_Load(void)
{

  /* This must be static. It is a struct. */
  /* The name in strings is the name by which it will be called from IDL and
     MUST BE CAPITALIZED 
     5th parameter will say if it accepts keywords and some other flags 
     For more info see page 325 of external dev. guide */
  static IDL_SYSFUN_DEF2 func_addr[] = {
    { (IDL_SYSRTN_GENERIC) total_int, "TOTAL_INT", 0, IDL_MAXPARAMS, 0, 0},
  };

  /* False means it is not a function */
  return IDL_SysRtnAdd(func_addr, IDL_TRUE, ARRLEN(func_addr));

}
