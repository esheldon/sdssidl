#include "applyPixelMaskUtil.h"

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
