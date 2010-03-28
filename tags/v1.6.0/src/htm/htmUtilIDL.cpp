#include "SpatialVector.h"
#include "SpatialInterface.h"
#include "VarStr.h"
#include "idl_export.h"
#include "htmUtilIDL.h"

int getIntVal(IDL_VPTR intVptr)
{

  int val;
  IDL_VPTR intCvt;

  if ( (intVptr->flags & IDL_V_ARR) != 0)
    return -1;

  if (intVptr->type != IDL_TYP_LONG)
    {
      intCvt = IDL_CvtLng(1, &intVptr);
      val = intCvt->value.l;
      IDL_Deltmp(intCvt);
    }
  else
    val = intVptr->value.l;

  return val;
}

double getDblVal(IDL_VPTR dblVptr)
{

  double val;
  IDL_VPTR dblCvt;

  if ( (dblVptr->flags & IDL_V_ARR) != 0)
    return -1;

  if (dblVptr->type != IDL_TYP_DOUBLE)
    {
      dblCvt = IDL_CvtDbl(1, &dblVptr);
      val = dblCvt->value.d;
      IDL_Deltmp(dblCvt);
    }
  else
    val = dblVptr->value.d;

  return val;
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

int getIDLLongPtr(IDL_VPTR IDLLongVptr, IDL_LONG **IDLLongPtr, IDL_MEMINT *num)
{

  if (IDLLongVptr->type != IDL_TYP_LONG)
    return 0;
  else
    {
      IDL_VarGetData(IDLLongVptr, num, (char **) IDLLongPtr, TRUE);	
      return 1;
    }

}
