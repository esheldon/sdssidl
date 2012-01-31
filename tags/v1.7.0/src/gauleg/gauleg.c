/*
   From numerical recipes.  The C version is 22 times faster than a braindead
   implementation with loops in IDL.

   IDL DLM implementation: Erin Sheldon UofChicago.  15-Nov-2004



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

#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include "gauleg.h"
#include "idl_export.h"

void gauleg(int argc, IDL_VPTR argv[])
{

    /* variables pointing to the input */
    IDL_VPTR x1_src, x2_src;
    IDL_VPTR npts_src;

    /* x and w point to the output. xtmp and wtmp will be variables modified 
       locally */
    IDL_VPTR x, w, xtmp, wtmp;
    double *x_d, *w_d;

    /* will hold the output array dimensions */
    IDL_ARRAY_DIM dim;

    /* hold the values from inputs for easier notation */
    double x1, x2;
    IDL_MEMINT npts;

    /* gauleg algorithm variables */
    int i, j, m;
    double xm, xl, z1, z, p1, p2, p3, pp=0, pi, EPS;

    /* check arguments */

    if (argc != 5)
    {
        IDL_Message(IDL_M_NAMED_GENERIC, IDL_MSG_LONGJMP,
                "Syntax: gauleg, x1, x2, npts, x, w");
    }

    x1_src = argv[0];
    x2_src = argv[1];
    npts_src = argv[2];
    x = argv[3];
    w = argv[4];

    IDL_ENSURE_SCALAR(x1_src);
    IDL_ENSURE_SIMPLE(x1_src);

    IDL_ENSURE_SCALAR(x2_src);
    IDL_ENSURE_SIMPLE(x2_src);

    IDL_ENSURE_SCALAR(npts_src);
    IDL_ENSURE_SIMPLE(npts_src);

    /* convert to the correct type if needed */
    npts = IDL_MEMINTScalar(npts_src);
    x1 = IDL_DoubleScalar(x1_src);
    x2 = IDL_DoubleScalar(x2_src);

    /* allocate the space for our output variables */
    dim[0] = npts;

    x_d = 
        (double *) IDL_MakeTempArray(IDL_TYP_DOUBLE, 1, dim, 
                IDL_ARR_INI_NOP, &xtmp);

    w_d = 
        (double *) IDL_MakeTempArray(IDL_TYP_DOUBLE, 1, dim, 
                IDL_ARR_INI_NOP, &wtmp);

    /* Run the gauleg algorithm */

    EPS = 3.e-11;
    pi = 3.1415927;

    m = (npts + 1)/2;

    xm = (x1 + x2)/2.0;
    xl = (x2 - x1)/2.0;
    z1 = 0.0;

    for (i=1; i<= m; ++i) 
    {

        z=cos( pi*(i-0.25)/(npts+.5) );

        while (abszdiff(z-z1) > EPS) 
        {
            p1 = 1.0;
            p2 = 0.0;
            for (j=1; j <= npts;++j)
            {
                p3 = p2;
                p2 = p1;
                p1 = ( (2.0*j - 1.0)*z*p2 - (j-1.0)*p3 )/j;
            }
            pp = npts*(z*p1 - p2)/(z*z -1.);
            z1=z;
            z=z1 - p1/pp;

        }

        x_d[i-1] = xm - xl*z;
        x_d[npts+1-i-1] = xm + xl*z;
        w_d[i-1] = 2.0*xl/( (1.-z*z)*pp*pp );
        w_d[npts+1-i-1] = w_d[i-1];

    }

    /* 
       because xtmp and wtmp are temporary, no copy is done.  The new array
       simply points to the data from xtmp,wtmp and the wrapper IDL_Array
       structure for those is free.  This is very efficient. 
       */

    IDL_VarCopy(xtmp, x);
    IDL_VarCopy(wtmp, w);

}

double abszdiff(double zdiff) 

{
    if (zdiff < 0.0) 
        return(-1.*zdiff);
    else
        return(zdiff);
}


#define ARRLEN(arr) (sizeof(arr)/sizeof(arr[0]))

int IDL_Load(void)
{

    /* This must be static. It is a struct. */
    /* The name in strings is the name by which it will be called from IDL and
       MUST BE CAPITALIZED 
       5th parameter will say if it accepts keywords and some other flags 
       For more info see page 325 of external dev. guide */
    static IDL_SYSFUN_DEF2 procedure_addr[] = {
        { (IDL_SYSRTN_GENERIC) gauleg, "GAULEG", 0, IDL_MAXPARAMS, 0, 0},
    };

    /* False means it is not a function */
    return IDL_SysRtnAdd(procedure_addr, IDL_FALSE, ARRLEN(procedure_addr));

}
