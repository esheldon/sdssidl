#define SUCCESS 0
#define FAILURE 1

int sphPolyCompNParams(int argc);
int getDblPtr(IDL_VPTR dblVptr, double **dblPtr, IDL_MEMINT *num);
void sphPolyCompErrOut(char *message, IDL_VPTR outVar, int statusVal);
static void sphPolyCompSetStatus(int statusVal);

typedef struct {
  IDL_KW_RESULT_FIRST_FIELD; /* Must be first entry in structure */

  IDL_VPTR poly_area;
  int poly_area_there;

  IDL_VPTR poly_id;
  int poly_id_there;

  IDL_VPTR status;
  int status_there;

  IDL_VPTR total_area;
  int total_area_there;

  IDL_VPTR total_effarea;
  int total_effarea_there;

  IDL_LONG verbose;
  int verbose_there;

} KW_RESULT;

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

  {"POLY_AREA", IDL_TYP_UNDEF, 1, IDL_KW_OUT | IDL_KW_ZERO, 
   (int *) IDL_KW_OFFSETOF(poly_area_there), (char *) IDL_KW_OFFSETOF(poly_area) },

  /* I'm using out here becuase otherwise my nparams program breaks */
  {"POLY_ID", IDL_TYP_UNDEF, 1, IDL_KW_OUT | IDL_KW_ZERO, 
   (int *) IDL_KW_OFFSETOF(poly_id_there), (char *) IDL_KW_OFFSETOF(poly_id) },

  {"STATUS", IDL_TYP_UNDEF, 1, IDL_KW_OUT | IDL_KW_ZERO, 
   (int *) IDL_KW_OFFSETOF(status_there), (char *) IDL_KW_OFFSETOF(status) },


  {"TOTAL_AREA", IDL_TYP_UNDEF, 1, IDL_KW_OUT | IDL_KW_ZERO, 
   (int *) IDL_KW_OFFSETOF(total_area_there), (char *) IDL_KW_OFFSETOF(total_area) },

  {"TOTAL_EFFAREA", IDL_TYP_UNDEF, 1, IDL_KW_OUT | IDL_KW_ZERO, 
   (int *) IDL_KW_OFFSETOF(total_effarea_there), (char *) IDL_KW_OFFSETOF(total_effarea) },


  {"VERBOSE", IDL_TYP_LONG, 1, IDL_KW_ZERO, 
   (int *) IDL_KW_OFFSETOF(verbose_there), (char *) IDL_KW_OFFSETOF(verbose) },

  { NULL }
};

