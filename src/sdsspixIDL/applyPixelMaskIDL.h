#define SUCCESS 0
#define FAILURE 1

int applyPixelMaskNParams(int argc);
void applyPixelMaskErrOut(char *message, IDL_VPTR outVar, int statusVal);
int getMaxAngle(int numLam, float **maxAngle, IDL_MEMINT *numAngle);
static void applyPixelMaskSetStatus(int statusVal);
void applyPixelMaskSetArea();



int ApplyMask(double *lambdav, double *etav, unsigned long n_gal, 
        short *is_unmaskedv, unsigned long *n_unmasked);

typedef struct {
  IDL_KW_RESULT_FIRST_FIELD; /* Must be first entry in structure */

  IDL_VPTR area;
  int area_there;

  IDL_VPTR maxAngle;
  int maxAngle_there;

  IDL_VPTR status;
  int status_there;

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

  {"AREA", IDL_TYP_UNDEF, 1, IDL_KW_OUT | IDL_KW_ZERO, 
   (int *) IDL_KW_OFFSETOF(area_there), (char *) IDL_KW_OFFSETOF(area) },

  /* I'm using out here becuase otherwise my nparams program breaks */
  {"MAXANGLE", IDL_TYP_UNDEF, 1, IDL_KW_OUT | IDL_KW_ZERO, 
   (int *) IDL_KW_OFFSETOF(maxAngle_there), (char *) IDL_KW_OFFSETOF(maxAngle) },

  {"STATUS", IDL_TYP_UNDEF, 1, IDL_KW_OUT | IDL_KW_ZERO, 
   (int *) IDL_KW_OFFSETOF(status_there), (char *) IDL_KW_OFFSETOF(status) },

  {"VERBOSE", IDL_TYP_LONG, 1, IDL_KW_ZERO, 
   (int *) IDL_KW_OFFSETOF(verbose_there), (char *) IDL_KW_OFFSETOF(verbose) },

  { NULL }
};

