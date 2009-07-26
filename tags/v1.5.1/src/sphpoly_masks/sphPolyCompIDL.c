/*
  Modified version of Andreas Berlinds spherical polygon code for linking 
  to IDL via the DLM mechanism

  -Syntax: completeness = 
                 sphpoly_comp(ra, dec, maskFile, 
                              poly_area=, poly_id=, status=, /verbose)

*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "idl_export.h"
#include "mask.h"


#include "sphPolyCompIDL.h"

/* Structure that holds the input keywords.  See htmIntersectIDL.h */
KW_RESULT kw;

IDL_VPTR sphPolyComp(int argc, IDL_VPTR *argv, char *argk) {

  /* Input IDL_VARIABLES */
  IDL_VPTR raVptr, decVptr, maskFileVptr;

  /* Pointers to arrays */
  double *RA, *DEC;
  char *maskFile;

  IDL_MEMINT numRA, numDEC;

  /* Keyword */
  short verbose;

  /* Output variable */
  IDL_VPTR compVptr;
  double *comp;

  IDL_VPTR poly_id_tmpVptr;
  IDL_VPTR poly_area_tmpVptr;
  IDL_LONG *poly_id=NULL;      /* The polygon id of each point */
  double *poly_area=NULL;       /* Area of this polygon */

  int id;
  double area;

  /* For creating the output variable */
  IDL_ARRAY_DIM dim;
  IDL_MEMINT n_dim;

  double x[3];            /* x,y,z version of RA/DEC on unit sphere */
  double ra, dec;       /* temporary variables to hold RA[i],DEC[i] */
  double theta, phi;      /* convert RA/DEC to theta phi */
  int i;                 /* loop variable */

  int retval;

  double total_area, total_effarea;

  /***************** Process keywords *****************/
  (void) IDL_KWProcessByOffset(argc, argv, argk, kw_pars, 
			       (IDL_VPTR *) 0, 1, &kw);

  /* Check number of args */
  if (sphPolyCompNParams(argc) < 3) 
    {
      sphPolyCompErrOut("-Syntax: completeness = sphpoly_comp(ra, dec, maskFile, poly_area=, poly_id=, status=, /verbose)", compVptr=IDL_Gettmp(), FAILURE);
      return(compVptr);
    }

  raVptr = argv[0];
  decVptr = argv[1];
  maskFileVptr = argv[2];

  /*-----------------------------------------------------------------------*/
  /*                    Extract pointers to ra/dec                         */
  /*-----------------------------------------------------------------------*/

  if (!getDblPtr(raVptr, &RA, &numRA))
    { 
      sphPolyCompErrOut("RA must be of type DOUBLE", 
			compVptr=IDL_Gettmp(), FAILURE);
      return(compVptr);
    }

  if (!getDblPtr(decVptr, &DEC, &numDEC))
    { 
      sphPolyCompErrOut("DEC must be of type DOUBLE", 
			compVptr=IDL_Gettmp(), FAILURE);
      return(compVptr);
    }
  /* Do they have the same length? */
  if (numRA != numDEC)
    {
      sphPolyCompErrOut("RA and DEC must be the same length",
			compVptr=IDL_Gettmp(), FAILURE);
      return(compVptr);
    }

  /********************* Extract the file name *********************/
  if (maskFileVptr->type != IDL_TYP_STRING)
    {
      sphPolyCompErrOut("Mask Filename must be a string",
			compVptr=IDL_Gettmp(), FAILURE);
      return(compVptr);
    }
  else 
    maskFile = maskFileVptr->value.str.s;

  /* Should we print informative messages? */
  verbose = 0;
  if (kw.verbose_there) verbose = kw.verbose;


  if (verbose) 
    printf("MaskFile: %s\n\n", maskFile);

  /*--------------------  Load-Mask-Info----------------------------*/

  loadmask_info(maskFile, &total_area, &total_effarea);

  /*------------------ Copy in if these keywords are present ------*/
  if (kw.total_area_there)
    {
      IDL_StoreScalarZero(kw.total_area, IDL_TYP_DOUBLE);
      kw.total_area->value.d = total_area;
    }

  if (kw.total_effarea_there)
    {
      IDL_StoreScalarZero(kw.total_effarea, IDL_TYP_DOUBLE);
      kw.total_effarea->value.d = total_effarea;
    }

  /*-----------------------------------------------------------------------*/
  /* Set the output variables                                              */
  /* Create space for our output variable                                  */
  /*-----------------------------------------------------------------------*/

  n_dim=1;
  dim[0] = numRA;
  comp = 
    (double *) IDL_MakeTempArray(IDL_TYP_DOUBLE, n_dim, dim, 
				 IDL_ARR_INI_NOP, &compVptr);

  if (kw.poly_area_there)
    poly_area = 
      (double *) IDL_MakeTempArray(IDL_TYP_DOUBLE, n_dim, dim, 
				   IDL_ARR_INI_NOP, &poly_area_tmpVptr);

  if (kw.poly_id_there)
    poly_id = 
      (IDL_LONG *) IDL_MakeTempArray(IDL_TYP_LONG, n_dim, dim, 
				     IDL_ARR_INI_NOP, &poly_id_tmpVptr);

  /*-----------------------------------------------------------------------*/
  /* Calculate the completeness                                            */
  /*-----------------------------------------------------------------------*/

  for(i=0;i<numRA;i++) {

    ra  = RA[i];
    dec = DEC[i];
    
    theta = (90-dec)*pi/180.0 ;
    phi = ra*pi/180.0 ;
    
    x[0] = sin(theta)*cos(phi) ;
    x[1] = sin(theta)*sin(phi) ;
    x[2] = cos(theta) ;
    
    retval = completeness_id(x, &comp[i], &id, &area);

    if (kw.poly_area_there)
      poly_area[i] = area;    

    if (kw.poly_id_there)
      poly_id[i] = (IDL_LONG) id;

  }

  /*-----------------------------------------------------------------------*/
  /* copy into keywords. This just copies pointers, its fast               */
  /* Note, since these are  temporary variables, they are also released    */
  /*-----------------------------------------------------------------------*/

  if (kw.poly_area_there)
    IDL_VarCopy(poly_area_tmpVptr, kw.poly_area);

  if (kw.poly_id_there)
    IDL_VarCopy(poly_id_tmpVptr, kw.poly_id);

  /* Clean up the keyword info */
  IDL_KW_FREE;

  sphPolyCompSetStatus(SUCCESS);

  return(compVptr);

}

int sphPolyCompNParams(int argc)
{
  int nKeywords;

  nKeywords = 
    kw.poly_area_there + kw.poly_id_there + 
    kw.status_there + 
    kw.total_area_there + kw.total_effarea_there + 
    kw.verbose_there;

  return 
    argc - nKeywords;
}

int getDblPtr(IDL_VPTR dblVptr, double **dblPtr, IDL_MEMINT *num)
{

  if (dblVptr->type != IDL_TYP_DOUBLE)
    return 0;
  else
    {
      IDL_VarGetData(dblVptr, num, (char **) dblPtr, TRUE);	
      return 1;
    }

}

/*/////////////////////////////////////////////////////////
  // Print error message, set status, and set output var
  /////////////////////////////////////////////////////////*/

void sphPolyCompErrOut(char *message, IDL_VPTR outVar, int statusVal)
{

  IDL_VPTR errVal;
  errVal = IDL_Gettmp();
  errVal->type = IDL_TYP_INT;
  errVal->value.i = -1;

  IDL_Message(IDL_M_NAMED_GENERIC, IDL_MSG_INFO, message);

  if (kw.status_there)
    sphPolyCompSetStatus(statusVal);

  IDL_VarCopy(errVal, outVar);

  /* Clean up the keyword info */
  IDL_KW_FREE;

}

/* set the status if it is there */
static void
sphPolyCompSetStatus(int statusVal)
{

  if (kw.status_there) {
    /* This frees any existing memory and sets type to INT with value zero */
    IDL_StoreScalarZero(kw.status, IDL_TYP_INT);
    kw.status->value.i = statusVal;
  }

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
    { (IDL_SYSRTN_GENERIC) sphPolyComp, "SPHPOLY_COMP", 0, 
      IDL_MAXPARAMS, IDL_SYSFUN_DEF_F_KEYWORDS, 0},
  };

  /* False means it is not a function */
  return IDL_SysRtnAdd(func_addr, IDL_TRUE, ARRLEN(func_addr));

}
