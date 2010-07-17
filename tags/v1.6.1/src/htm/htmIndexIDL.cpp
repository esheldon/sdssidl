//#     --------------------------------------------------------------------
//#     Return the HTM indices (leafids) at a given depth for a list of 
//#     ra/dec pairs.
//#     
//#     Hacked to be a used as Dynamially Loadable Module (DLM) and just look 
//#     up a list of ra's and dec's.  Renamed to
//#           htmIndexIDL.cpp    18-Nov-2004 Erin Scott Sheldon UofChicago
//#
//#     usage from idl:
//#        leafid = htm_index(ra, dec, depth, status=status)
//#
//#     The ra,dec must be double of same lenght.  Depth must be a scalar.
//#     The returned index, or leafid array, is a 64-bit unsigned integer. 
//#     This precision is required for depth >= 15.  Depth of 14 requires 
//#     32-bit unsigned, and 13 only 32-bit signed.
//#
//#     --------------------------------------------------------------------
//#     Original Comments:
//#     specify a point on the sphere, return its ID/Name to a certain depth
//#
//#
//#     Author:         Peter Z. Kunszt
//#
//#     Date:           October 15, 1999
//#     Modified for linking to IDL, Erin Sheldon, UofChicago
//#
//#
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
#include "SpatialVector.h"
#include "SpatialInterface.h"
#include "VarStr.h"
#include <stdlib.h>
#include "idl_export.h"

#include "htmIndexIDL.h"
#include "htmUtilIDL.h"

/* Structure that holds the input keywords.  See htmIndexIDL.h */
KW_RESULT kw;

IDL_VPTR 
htmIndex(int argc, IDL_VPTR *argv, char *argk) {

//*******************************************************
//
// Initialization
//
//*******************************************************


  // These point to the input IDL_VARIABLES 
  IDL_VPTR raVptr, decVptr, depthVptr;

  // These point to the data regions of those variables 
  double *ra, *dec;
  // This is a copy of depth value 
  int depth;

  IDL_MEMINT numRA, numDEC;

  // This is the return value.  index points to the data region 
  IDL_VPTR indexVptr;
  uint64 *index;

  // We load the htm info here
  htmInterface *htm;

  // for looping, etc.
  IDL_MEMINT j;

  ///////////////////////////////////////////////////////////
  // Process the keywords 
  ///////////////////////////////////////////////////////////

  (void) IDL_KWProcessByOffset(argc, argv, argk, kw_pars, 
			       (IDL_VPTR *) 0, 1, &kw);

  ///////////////////////////////////////////////////////////
  // Check the arguments
  ///////////////////////////////////////////////////////////

  if (htmIndexNParams(argc) != 3) 
    {
      char *message = 
	"-Syntax: index = htm_index(ra, dec, depth, status=)\nRA/DEC must be double";
      htmIndexErrOut(message, indexVptr=IDL_Gettmp(), FAILURE);
      return(indexVptr);
    }

  // set the input values 
  raVptr    = argv[0];
  decVptr   = argv[1];
  depthVptr = argv[2];

  // Extract value of depth
  depth = getIntVal(depthVptr);
  if (depth < 0)
    {
      htmIndexErrOut("Depth must be a scalar", 
		     indexVptr=IDL_Gettmp(), FAILURE);
      return(indexVptr);
    }

  // Extract pointers to ra/dec data
  if (!getDblPtr(raVptr, &ra, &numRA))
    { 
      htmIndexErrOut("RA must be of type double", 
		     indexVptr=IDL_Gettmp(), FAILURE);
      return(indexVptr);
    }

  if (!getDblPtr(decVptr, &dec, &numDEC))
    { 
      htmIndexErrOut("DEC must be of type double", 
  		     indexVptr=IDL_Gettmp(), FAILURE);
      return(indexVptr);
    }

  if (numRA != numDEC) 
    {
      htmIndexErrOut("RA and DEC must be the same length",
		     indexVptr=IDL_Gettmp(), FAILURE);
      return(indexVptr);
    }

  // Done with type checking.  Create the index variable with same
  // dimenstions as input ra

  index = 
    (uint64 *) IDL_VarMakeTempFromTemplate(raVptr, // source
					  IDL_TYP_ULONG64, // type for output
					  NULL, // Structure def
					  &indexVptr, // destination
					  FALSE); // Zero the values?'


  try {

    htm = new htmInterface( (size_t) depth );

    /////////////////////////////////////////////////////////
    // Lookup all the ra,dec positions
    /////////////////////////////////////////////////////////


    // lookup id by ra dec
    for (j=0; j < numRA; ++j) {
      index[j] = (uint64 ) htm->lookupID(ra[j], dec[j]);  
    }

  } catch (SpatialException x) {
    char errMessage[100];
    sprintf(errMessage, "%s", x.what());
    htmIndexErrOut(errMessage, indexVptr=IDL_Gettmp(), FAILURE);
    return(indexVptr);
  }

  /* Clean up the keyword info */
  IDL_KW_FREE;
  htmIndexSetStatus(SUCCESS);
  return(indexVptr);
}


//////////////////////////////////////////////////////////////////////////////
// Extract values
//////////////////////////////////////////////////////////////////////////////


/* set the status if it is there */
static void
htmIndexSetStatus(int statusVal)
{

  if (kw.status_there) {
    /* This frees any existing memory and sets type to INT with value zero */
    IDL_StoreScalarZero(kw.status, IDL_TYP_INT);
    kw.status->value.i = statusVal;
  }

}

/*===========================================================================
 * htmIndexNParams
 * How many positional arguments were sent?
 *===========================================================================*/

int htmIndexNParams(int argc)
{

  int nKeywords;

  nKeywords = kw.status_there;

  return 
    argc - nKeywords;

}

/////////////////////////////////////////////////////////
// Print error message, set status, and set output var
/////////////////////////////////////////////////////////

void htmIndexErrOut(char *message, IDL_VPTR outVar, int statusVal)
{

  IDL_VPTR errVal;
  errVal = IDL_Gettmp();
  errVal->type = IDL_TYP_INT;
  errVal->value.i = -1;

  IDL_Message(IDL_M_NAMED_GENERIC, IDL_MSG_INFO, message);

  if (kw.status_there)
    htmIndexSetStatus(statusVal);

  IDL_VarCopy(errVal, outVar);
  
  /* Clean up the keyword info */
  IDL_KW_FREE;

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
    { (IDL_SYSRTN_GENERIC) htmIndex, "HTM_INDEX", 0, IDL_MAXPARAMS, 
      IDL_SYSFUN_DEF_F_KEYWORDS, 0},
  };

  /* The false means it is not a function */
  return IDL_SysRtnAdd(func_addr, IDL_TRUE, ARRLEN(func_addr));

}

