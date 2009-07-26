//#     --------------------------------------------------------------------
//# 
//#     htm_intersect
//# 
//#     Find all HTM triangles at a given depth that intersect a circle of 
//#     radius "angle" centered at ra/dec.
//# 
//#     Hacked up to link into IDL as a DLM. Had to alter classes
//#           SpatialConstraint, SpatialConvex, SpatialDomain so I could just 
//#           send an ra,dec,dist in without needing a file.  
//#
//#     usage from idl:
//#          idlist = htm_intersect(ra, dec, depth, angle, status=)
//#     
//#          ra/dec/depth/angle must be scalars.
//#
//#    idlist is returned as an array of 64-bit unsigned integers. 
//#    This precision is required for depth >= 15.  Depth of 14 requires 
//#    32-bit unsigned, and 13 only 32-bit signed.
//# 
//#     Original author info:
//#         Author:         Peter Z. Kunszt
//#         Date:           October 15, 1999
//#     Modified for linking to IDL, Erin Sheldon, UofChicago
//#
//# Here is the original copyright info:
//#
//# (c) Copyright The Johns Hopkins University 1999
//# All Rights Reserved
//#
//# The software and information contained herein are proprietary to The
//# Johns Hopkins University, Copyright 1999.  This software is furnished
//# pursuant to a written license agreement and may be used, copied,
//# transmitted, and stored only in accordance with the terms of such
//# license and with the inclusion of the above copyright notice.  This
//# software and information or any other copies thereof may not be
//# provided or otherwise made available to any other person.
//#
//#

#include "SpatialInterface.h"
#include "SpatialDomain.h"
#include "VarStr.h"
#include <fstream>
#include <time.h>
#include <stdlib.h>
#include "idl_export.h"

#include "htmIntersectIDL.h"
#include "htmUtilIDL.h"

/* Structure that holds the input keywords.  See htmIntersectIDL.h */
KW_RESULT kw;

IDL_VPTR
htmIntersect(int argc, IDL_VPTR *argv, char *argk) {

  // These point to the input IDL_VARIABLES 
  IDL_VPTR raVptr, decVptr, depthVptr, angleVptr;

  // These are copies of the values
  double ra, dec;     // degrees
  int depth;
  double angle;       // radians
  
  // holds cos(angle)
  double dist;

  //IDL_MEMINT numRA, numDEC;

  // This is the return value.  index points to the data region 
  IDL_VPTR idlistVptr;
  uint64 *idlist;

  int savedepth=2;	// depth and stored depth
  size_t i;

  ///////////////////////////////////////////////////////////
  // Process the keywords 
  ///////////////////////////////////////////////////////////

  (void) IDL_KWProcessByOffset(argc, argv, argk, kw_pars, 
			       (IDL_VPTR *) 0, 1, &kw);

  if (htmIntersectNParams(argc) != 4) 
    {
      char *message = 
	"-Syntax: idlist = htm_intersect(ra, dec, depth, angle, status=)\nAngle in radians";
      htmIntersectErrOut(message, idlistVptr=IDL_Gettmp(), FAILURE);
 
 
      return(idlistVptr);
    }

  raVptr    = argv[0];
  decVptr   = argv[1];
  depthVptr = argv[2];
  angleVptr  = argv[3];

  ra    = getDblVal(raVptr);
  dec   = getDblVal(decVptr);
  depth = getIntVal(depthVptr);
  angle  = getDblVal(angleVptr);
  dist = cos(angle);

  // Some checking
  if(savedepth < 1 || savedepth > 8) {
    htmIntersectErrOut("savedepth should be between 1 and 8", 
		       idlistVptr=IDL_Gettmp(), FAILURE);
    return(idlistVptr);
  }
  if(depth < 1 || depth > HTMMAXDEPTH) {
    char errMessage[100];
    sprintf(errMessage, "depth should be between 1 and %d",HTMMAXDEPTH);
    htmIntersectErrOut(errMessage, 
		       idlistVptr=IDL_Gettmp(), FAILURE);
    return(idlistVptr);
  }

  try {
    // construct index with given depth and savedepth
    htmInterface htm(depth,savedepth);  // generate htm interface
    const SpatialIndex &index = htm.index();

    // Read in domain and echo it to screen
    SpatialDomain domain;    // initialize empty domain
    domain.setRaDecD(ra,dec,dist); //put in ra,dec,d E.S.S.

    //////////////////////////////
    // Intersection
    //////////////////////////////

    ValVec<uint64> plist, flist;	// List results
    //const ValVec<htmRange> *rlist;
        
    domain.intersect(&index,plist,flist);	  // intersect with list
      
    /////////////////////////////////////
    // Save the result in idlist
    /////////////////////////////////////
      
    // Make the idl variable
    int nTotal = flist.length() + plist.length();

    IDL_ARRAY_DIM dim;
    IDL_MEMINT n_dim=1;
    dim[0] = nTotal;
    idlist = (uint64 *) IDL_MakeTempArray(IDL_TYP_ULONG64, n_dim, dim, 
					  IDL_ARR_INI_NOP, &idlistVptr);

    // ----------- FULL NODES -------------
    int idCount = 0;
    for(i = 0; i < flist.length(); i++){  // just loop this list
      idlist[idCount] = (uint64 )flist(i);
      idCount++;
    }
    // ----------- Partial Nodes ----------
    for(i = 0; i < plist.length(); i++){  // just loop this list
      idlist[idCount] = (uint64 )plist(i);
      idCount++;
    }
    
  } catch (SpatialException &x) {
    printf("%s\n",x.what());
  }

  /* Clean up the keyword info */
  IDL_KW_FREE;

  htmIntersectSetStatus(SUCCESS);
  return(idlistVptr);

}

/////////////////////////////////////////////////////////
// Print error message, set status, and set output var
/////////////////////////////////////////////////////////

void htmIntersectErrOut(char *message, IDL_VPTR outVar, int statusVal)
{

  IDL_VPTR errVal;
  errVal = IDL_Gettmp();
  errVal->type = IDL_TYP_INT;
  errVal->value.i = -1;

  IDL_Message(IDL_M_NAMED_GENERIC, IDL_MSG_INFO, message);

  if (kw.status_there)
    htmIntersectSetStatus(statusVal);

  IDL_VarCopy(errVal, outVar);

  /* Clean up the keyword info */
  IDL_KW_FREE;

}

/* set the status if it is there */
static void
htmIntersectSetStatus(int statusVal)
{

  if (kw.status_there) {
    /* This frees any existing memory and sets type to INT with value zero */
    IDL_StoreScalarZero(kw.status, IDL_TYP_INT);
    kw.status->value.i = statusVal;
  }

}


/*===========================================================================
 * htmIntersectNParams
 * How many positional arguments were sent?
 *===========================================================================*/

int htmIntersectNParams(int argc)
{

  int nKeywords;

  nKeywords = kw.status_there;

  return 
    argc - nKeywords;

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
  static IDL_SYSFUN_DEF2 func_addr[] = {
    { (IDL_SYSRTN_GENERIC) htmIntersect, "HTM_INTERSECT", 0, IDL_MAXPARAMS, 
      IDL_SYSFUN_DEF_F_KEYWORDS, 0},
  };

  /* The false means it is not a function */
  return IDL_SysRtnAdd(func_addr, IDL_TRUE, ARRLEN(func_addr));

}

