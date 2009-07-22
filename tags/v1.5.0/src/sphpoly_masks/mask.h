/* definitions */

#define pi 3.1415926535898
#define sqr(x) ((x)*(x))
#define nrmax 200000

/* Function Prototypes */

void loadmask(char *maskfile);
void loadmask_info(char *maskfile, double *area, double *effarea);

double dot1(double *A, double *B);

double completeness(double *x);
int completeness_id(double *x, 
		    double *comp, 
		    int *poly_id, 
		    double *poly_area);

int polygon(double *x);

int overlap(char *polyfile, int ipolygon);

int boundary(char *polyfile, int ipolygon);

int chunk(char *polyfile, int ipolygon);
